module Graphics.Seangine.Monad.Vulkan
  ( VulkanHandles (..),
    Vulkan (..),
    askVulkan,
    runVulkan,
  ) where

import Graphics.Seangine.HasVulkan (HasVulkan (..))

import Control.Monad.Trans.Resource
import Data.Vector (Vector)
import UnliftIO (MonadUnliftIO (..))
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR, SurfaceCapabilitiesKHR, SurfaceFormatKHR)
import VulkanMemoryAllocator

newtype Vulkan a = Vulkan {unVulkan :: ReaderT VulkanHandles (ResourceT IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadFail,
      MonadIO,
      MonadReader VulkanHandles,
      MonadResource
    )

data VulkanHandles = VulkanHandles
  { vhDataDir :: FilePath,
    vhInstance :: Instance,
    vhPhysicalDevice :: PhysicalDevice,
    vhDevice :: Device,
    vhAllocator :: Allocator,
    vhGraphicsQueue :: Queue,
    vhGraphicsQueueFamily :: Word32,
    vhPresentQueue :: Queue,
    vhPresentQueueFamily :: Word32,
    vhSurfaceCapabilities :: SurfaceCapabilitiesKHR,
    vhSurfaceFormats :: Vector SurfaceFormatKHR,
    vhPresentModes :: Vector PresentModeKHR,
    vhCommandPool :: CommandPool
  }

instance HasVulkan Vulkan where
  getDataDir = Vulkan $ asks vhDataDir
  getInstance = Vulkan $ asks vhInstance
  getPhysicalDevice = Vulkan $ asks vhPhysicalDevice
  getDevice = Vulkan $ asks vhDevice
  getAllocator = Vulkan $ asks vhAllocator
  getGraphicsQueue = Vulkan $ asks vhGraphicsQueue
  getGraphicsQueueFamily = Vulkan $ asks vhGraphicsQueueFamily
  getPresentQueue = Vulkan $ asks vhPresentQueue
  getPresentQueueFamily = Vulkan $ asks vhPresentQueueFamily
  getSurfaceCapabilities = Vulkan $ asks vhSurfaceCapabilities
  getSurfaceFormats = Vulkan $ asks vhSurfaceFormats
  getPresentModes = Vulkan $ asks vhPresentModes
  getCommandPool = Vulkan $ asks vhCommandPool

instance MonadUnliftIO Vulkan where
  withRunInIO a = Vulkan $ withRunInIO (\r -> a (r . unVulkan))

askVulkan :: Vulkan VulkanHandles
askVulkan = Vulkan ask

runVulkan :: VulkanHandles -> Vulkan a -> ResourceT IO a
runVulkan handles (Vulkan action) = usingReaderT handles action
