module Graphics.Seangine.Config.VulkanHandles
  ( VulkanHandles (..),
    HasVulkan (..),
    noAllocationCallbacks,
    noPipelineCache,
  ) where

import Data.Vector (Vector)
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR, SurfaceCapabilitiesKHR, SurfaceFormatKHR)
import VulkanMemoryAllocator (Allocator (..))

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

noAllocationCallbacks :: Maybe AllocationCallbacks
noAllocationCallbacks = Nothing

noPipelineCache :: PipelineCache
noPipelineCache = NULL_HANDLE
