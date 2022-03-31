module Graphics.Seangine.GraphicsPipeline (withVulkanFrame) where

import Graphics.Seangine.Domain (Frame(..))
import Graphics.Seangine.Monad
import Graphics.Seangine.Swapchain (withSwapchain)
import Graphics.Seangine.Window (Window())

import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR())

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import GHC.Clock (getMonotonicTime)

withVulkanFrame :: Window -> SurfaceKHR -> Vulkan Frame
withVulkanFrame window surface = do
  device <- getDevice
  handles <- Vulkan ask
  allocator <- getAllocator
  swapchain <- withSwapchain window surface

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
