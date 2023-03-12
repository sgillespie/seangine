module Graphics.Seangine.HasVulkan
  ( HasVulkan (..),
    HasFrame (..),
    HasFrameInFlight (..),
    noAllocationCallbacks,
    noPipelineCache,
  ) where

import Graphics.Seangine.Frame

import Data.Vector (Vector)
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR, SurfaceCapabilitiesKHR, SurfaceFormatKHR)
import VulkanMemoryAllocator

class HasVulkan m where
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

class HasFrame m where
  getFrame :: m Frame

class HasFrame m => HasFrameInFlight m where
  getFrameInFlight :: m FrameInFlight

instance (Monad m, HasVulkan m) => HasVulkan (ReaderT r m) where
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

instance (Monad m, HasFrame m) => HasFrame (ReaderT r m) where
  getFrame = lift getFrame

instance (Monad m, HasFrameInFlight m) => HasFrameInFlight (ReaderT r m) where
  getFrameInFlight = lift getFrameInFlight

noAllocationCallbacks :: Maybe AllocationCallbacks
noAllocationCallbacks = Nothing

noPipelineCache :: PipelineCache
noPipelineCache = NULL_HANDLE
