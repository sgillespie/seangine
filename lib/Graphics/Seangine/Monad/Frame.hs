module Graphics.Seangine.Monad.Frame
  ( VulkanFrame (..),
    allocateVulkan,
    allocateVulkan_,
    runFrame,
  ) where

import Graphics.Seangine.Frame (Frame (..))
import Graphics.Seangine.HasVulkan (HasFrame (..), HasFrameInFlight (..))
import Graphics.Seangine.Monad.Vulkan (Vulkan ())

import Control.Monad.Trans.Resource
import Data.Vector ((!))
import UnliftIO (MonadUnliftIO (..), askRunInIO, toIO)

newtype VulkanFrame a = VulkanFrame {unFrame :: ReaderT Frame Vulkan a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadFail,
      MonadIO,
      MonadReader Frame
    )

instance HasFrame VulkanFrame where
  getFrame = VulkanFrame ask

instance HasFrameInFlight VulkanFrame where
  getFrameInFlight = do
    Frame {..} <- getFrame
    let index = fromIntegral fIndex
    pure $ fFramesInFlight ! (index `mod` fMaxFramesInFlight)

instance MonadUnliftIO VulkanFrame where
  withRunInIO a = VulkanFrame $ withRunInIO (\r -> a (r . unFrame))

instance MonadResource VulkanFrame where
  liftResourceT r = do
    st <- VulkanFrame $ asks (snd . fResources)
    liftIO $ runInternalState r st

-- | Allocates a Frame, and runs the action, returning a resource release key.
allocateVulkan
  :: VulkanFrame a
  -> (a -> VulkanFrame ())
  -> VulkanFrame (ReleaseKey, a)
allocateVulkan create destroy = do
  createIO <- toIO create
  run <- askRunInIO
  VulkanFrame $ allocate createIO (run . destroy)

-- | Resource-less version of `allocateVulkan`
allocateVulkan_
  :: VulkanFrame a
  -> VulkanFrame ()
  -> VulkanFrame (ReleaseKey, a)
allocateVulkan_ create destroy = allocateVulkan create (const destroy)

-- | Runs the given Frame monad using the provided frame. The resulting environment
--  contains the global Vulkan handles
runFrame :: Frame -> VulkanFrame a -> Vulkan a
runFrame frame (VulkanFrame f) = runReaderT f frame
