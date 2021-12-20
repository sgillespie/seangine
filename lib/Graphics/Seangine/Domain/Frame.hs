module Graphics.Seangine.Domain.Frame (Frame(..)) where

import Data.Vector
import Data.Word

import Control.Monad.Trans.Resource
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_swapchain
import VulkanMemoryAllocator (Allocation)

-- |Per-frame resources
data Frame = Frame
  { fIndex :: Word64,
    fStartTime :: Double,
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
