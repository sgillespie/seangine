module Graphics.Seangine.Monad.Vulkan
  ( VulkanHandles (..),
    Vulkan (),
    askVulkan,
    runVulkan,
  ) where

import Graphics.Seangine.Config
import Graphics.Seangine.Config.VulkanHandles (HasVulkan (..), VulkanHandles (..))

import Control.Monad.Trans.Resource
import UnliftIO (MonadUnliftIO (..))

newtype Vulkan a = Vulkan {unVulkan :: ReaderT Config (ResourceT IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadFail,
      MonadIO,
      MonadReader Config,
      MonadResource
    )

instance HasConfig Vulkan where
  getVulkanHandles = Vulkan $ asks cfgVulkanHandles
  getOptions = Vulkan $ asks cfgOptions

instance HasVulkan Vulkan where
  getDataDir = vhDataDir <$> getVulkanHandles
  getInstance = vhInstance <$> getVulkanHandles
  getPhysicalDevice = vhPhysicalDevice <$> getVulkanHandles
  getDevice = vhDevice <$> getVulkanHandles
  getAllocator = vhAllocator <$> getVulkanHandles
  getGraphicsQueue = vhGraphicsQueue <$> getVulkanHandles
  getGraphicsQueueFamily = vhGraphicsQueueFamily <$> getVulkanHandles
  getPresentQueue = vhPresentQueue <$> getVulkanHandles
  getPresentQueueFamily = vhPresentQueueFamily <$> getVulkanHandles
  getSurfaceCapabilities = vhSurfaceCapabilities <$> getVulkanHandles
  getSurfaceFormats = vhSurfaceFormats <$> getVulkanHandles
  getPresentModes = vhPresentModes <$> getVulkanHandles
  getCommandPool = vhCommandPool <$> getVulkanHandles

instance MonadUnliftIO Vulkan where
  withRunInIO a = Vulkan $ withRunInIO (\r -> a (r . unVulkan))

askVulkan :: Vulkan Config
askVulkan = Vulkan ask

runVulkan :: Config -> Vulkan a -> ResourceT IO a
runVulkan handles (Vulkan action) = usingReaderT handles action
