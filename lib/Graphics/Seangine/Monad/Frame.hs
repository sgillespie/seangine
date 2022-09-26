module Graphics.Seangine.Monad.Frame
  ( Frame(..),
    FrameInFlight(..),
    MonadFrame(..),
    MonadFrameInFlight(..),
    SeangineFrame(..),
    allocateVulkan,
    allocateVulkan_,
    runFrame
  ) where

import Graphics.Seangine.Monad.Instance
import Graphics.Seangine.Scene (MeshPrimitiveId(..))

import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Resource
import RIO
import RIO.Vector.Boxed.Partial
import Text.GLTF.Loader (Gltf(..))
import Data.Word (Word32(), Word64())
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_swapchain (SurfaceKHR(), SwapchainKHR())
import VulkanMemoryAllocator (Allocation())
import qualified Data.HashMap.Strict as Map

-- |Monad class containing all Vulkan swapchain and frame-level resources. They MUST
-- be recreated along with the swapchain.
class Monad m => MonadFrame m where
  getFrame :: m Frame

class Monad m => MonadFrameInFlight m where
  getFrameInFlight :: m FrameInFlight

instance MonadFrame m => MonadFrame (ReaderT r m) where
  getFrame = lift getFrame

data Frame = Frame
  { fIndex :: Word64,
    fStartTime :: Double,
    fScene :: Gltf,
    fSurface :: SurfaceKHR,
    fSwapchain :: SwapchainKHR,
    fImageExtent :: Extent2D,
    fRenderPass :: RenderPass,
    fFramebuffers :: Vector Framebuffer,
    fPipelineLayout :: PipelineLayout,
    fGraphicsPipeline :: Pipeline,
    fVertexBuffers :: Map.HashMap MeshPrimitiveId Buffer,
    fIndexBuffers :: Map.HashMap MeshPrimitiveId Buffer,
    fMaxFramesInFlight :: Int,
    fFramesInFlight :: Vector FrameInFlight,
    fResources :: (ReleaseKey, InternalState)
  }

data FrameInFlight = FrameInFlight
  { ffImageAvailable :: Semaphore,
    ffRenderFinished :: Semaphore,
    ffUniformBuffer :: (Buffer, Allocation),
    ffObjectBuffer :: (Buffer, Allocation),
    ffDescriptorSets :: Vector DescriptorSet,
    ffCommandPool :: CommandPool,
    ffCommandBuffer :: CommandBuffer,
    ffGpuWork :: Fence
  }

newtype SeangineFrame a
  = SeangineFrame { unFrame :: ReaderT Frame SeangineInstance a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadFail,
            MonadIO,
            MonadInstance
           )

instance MonadFrame SeangineFrame where
  getFrame = SeangineFrame ask

instance MonadFrameInFlight SeangineFrame where
  getFrameInFlight = do
    Frame{..} <- getFrame
    let index = fromIntegral fIndex
    return $ fFramesInFlight ! (index `mod` fMaxFramesInFlight)
  
instance MonadUnliftIO SeangineFrame where
  withRunInIO a = SeangineFrame $ withRunInIO (\r -> a (r . unFrame))

instance MonadResource SeangineFrame where
  liftResourceT r = do
    i <- SeangineFrame $ asks (snd . fResources)
    liftIO $ runInternalState r i

-- |Allocates a Frame, and runs the action, returning a resource release key.
allocateVulkan
  :: SeangineFrame a
  -> (a -> SeangineFrame ())
  -> SeangineFrame (ReleaseKey, a)
allocateVulkan create destroy = do
  createIO <- toIO create
  run <- askRunInIO
  SeangineFrame $ allocate createIO (run . destroy)

-- |Resource-less version of `allocateVulkan`
allocateVulkan_
  :: SeangineFrame a
  -> SeangineFrame ()
  -> SeangineFrame (ReleaseKey, a)
allocateVulkan_ create destroy = allocateVulkan create (const destroy)

-- |Runs the given Frame monad using the provided frame. The resulting environment
-- contains the global Vulkan handles
runFrame :: Frame -> SeangineFrame a -> SeangineInstance a
runFrame frame (SeangineFrame f) = runReaderT f frame
