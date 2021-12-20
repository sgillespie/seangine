module Graphics.Seangine.Monad.VulkanFrame
  ( Frame(..),
    MonadFrame(..),
    VulkanFrame(..),
    allocateVulkan,
    allocateVulkan_,
    runFrame
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Resource

import Graphics.Seangine.Domain (Frame(..))
import Graphics.Seangine.Monad.Vulkan

-- |MonadFrame class. Contains reference to per-frame resources
class Monad m => MonadFrame m where
  getFrame :: m Frame

instance MonadFrame m => MonadFrame (ReaderT r m) where
  getFrame = lift getFrame

-- |Frame monad. Contains references to per-frame resources and Vulkan handles
newtype VulkanFrame a
  = VulkanFrame { unFrame :: ReaderT Frame Vulkan a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadFail,
            MonadIO,
            MonadVulkan
           )

instance MonadFrame VulkanFrame where
  getFrame = VulkanFrame ask
  
instance MonadUnliftIO VulkanFrame where
  withRunInIO a = VulkanFrame $ withRunInIO (\r -> a (r . unFrame))

instance MonadResource VulkanFrame where
  liftResourceT r = do
    i <- VulkanFrame $ asks (snd . fResources)
    liftIO $ runInternalState r i

-- |Allocates a Frame, and runs the action, returning a resource release key.
allocateVulkan
  :: VulkanFrame a
  -> (a -> VulkanFrame ())
  -> VulkanFrame (ReleaseKey, a)
allocateVulkan create destroy = do
  createIO <- toIO create
  run <- askRunInIO
  VulkanFrame $ allocate createIO (run . destroy)

-- |Resource-less version of `allocateVulkan`
allocateVulkan_
  :: VulkanFrame a
  -> VulkanFrame ()
  -> VulkanFrame (ReleaseKey, a)
allocateVulkan_ create destroy = allocateVulkan create (const destroy)

-- |Runs the given Frame monad using the provided frame. The resulting environment
-- contains the global Vulkan handles
runFrame :: Frame -> VulkanFrame a -> Vulkan a
runFrame frame (VulkanFrame f) = runReaderT f frame
