module Graphics.Seangine.GraphicsPipeline (withVulkanFrame) where

import Graphics.Seangine.Domain (Frame(..))
import Graphics.Seangine.Monad

import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainKHR(..))

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import GHC.Clock (getMonotonicTime)

withVulkanFrame :: SurfaceKHR -> Vulkan Frame
withVulkanFrame surface = do
  device <- getDevice
  handles <- Vulkan ask
  allocator <- getAllocator
  swapchain <- withSwapchain surface

  start <- liftIO getMonotonicTime
  
  return Frame
    { fIndex = 0,
      fStartTime = start,
      fSurface = surface,
      fSwapchain = swapchain,
      fImageExtent = undefined,
      fRenderPass = undefined,
      fFramebuffers = undefined,
      fPipelineLayout = undefined,
      fGraphicsPipeline = undefined,
      fImageAvailable = undefined,
      fRenderFinished = undefined,
      fVertexBuffer = undefined,
      fIndexBuffer = undefined,
      fUniformBuffers = undefined,
      fDescriptorSets = undefined,
      fResources = undefined,
      fGpuWork = undefined
    }

withSwapchain :: SurfaceKHR -> Vulkan SwapchainKHR
withSwapchain surface = undefined
