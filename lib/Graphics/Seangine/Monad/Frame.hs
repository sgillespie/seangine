module Graphics.Seangine.Monad.Frame
  ( Frame(..),
    MonadFrame(..),
    SeangineFrame(..),
    allocateVulkan,
    allocateVulkan_,
    runFrame
  ) where

import Graphics.Seangine.Monad.Instance

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Resource
import Text.GLTF.Loader (Gltf(..))
import Data.Vector (Vector())
import Data.Word (Word32(), Word64())
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_swapchain (SurfaceKHR(), SwapchainKHR())
import VulkanMemoryAllocator (Allocation())

-- |Monad class containing all Vulkan swapchain and frame-level resources. They MUST
-- be recreated along with the swapchain.
class Monad m => MonadFrame m where
  getFrame :: m Frame

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
    fImageAvailable :: Semaphore,
    fRenderFinished :: Semaphore,
    fVertexBuffer :: Buffer,
    fIndexBuffer :: Buffer,
    fUniformBuffers :: Vector (Buffer, Allocation),
    fDescriptorSets :: Word32 -> DescriptorSet,
    fResources :: (ReleaseKey, InternalState),
    fGpuWork :: Fence
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
