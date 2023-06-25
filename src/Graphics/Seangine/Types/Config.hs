module Graphics.Seangine.Types.Config
  ( SeangineConfig (..),
    SeangineOptions (..),
    VulkanHandles (..),
    HasSeangineOptions (..),
    HasVulkanHandles (..),
  ) where

import Data.Vector (Vector)
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface
  ( PresentModeKHR,
    SurfaceCapabilitiesKHR,
    SurfaceFormatKHR,
  )
import VulkanMemoryAllocator (Allocator (..))

data SeangineConfig = SeangineConfig
  { options :: SeangineOptions,
    handles :: VulkanHandles
  }

data SeangineOptions = SeangineOptions
  { optFile :: !FilePath,
    optDevice :: !(Maybe String),
    optDebug :: !Bool
  }
  deriving (Eq)

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

class HasSeangineOptions m where
  getOptFile :: m FilePath
  getOptDevice :: m (Maybe String)
  getOptDebug :: m Bool

class HasVulkanHandles m where
  getDataDir :: m FilePath
  getInstance :: m Instance
  getPhysicalDevice :: m PhysicalDevice
  getDevice :: m Device
  getAllocator :: m Allocator
  getGraphicsQueue :: m Queue
  getGraphicsQueueFamily :: m Word32
  getPresentQueue :: m Queue
  getPresentQueueFamily :: m Word32
  getSurfaceCapabilities :: m SurfaceCapabilitiesKHR
  getSurfaceFormats :: m (Vector SurfaceFormatKHR)
  getPresentModes :: m (Vector PresentModeKHR)
  getCommandPool :: m CommandPool

instance (Monad m, HasSeangineOptions m) => HasSeangineOptions (ReaderT r m) where
  getOptFile = lift getOptFile
  getOptDevice = lift getOptDevice
  getOptDebug = lift getOptDebug

instance (Monad m, HasVulkanHandles m) => HasVulkanHandles (ReaderT r m) where
  getDataDir = lift getDataDir
  getInstance = lift getInstance
  getPhysicalDevice = lift getPhysicalDevice
  getDevice = lift getDevice
  getAllocator = lift getAllocator
  getGraphicsQueue = lift getGraphicsQueue
  getGraphicsQueueFamily = lift getGraphicsQueueFamily
  getPresentQueue = lift getPresentQueue
  getPresentQueueFamily = lift getPresentQueueFamily
  getSurfaceCapabilities = lift getSurfaceCapabilities
  getSurfaceFormats = lift getSurfaceFormats
  getPresentModes = lift getPresentModes
  getCommandPool = lift getCommandPool
