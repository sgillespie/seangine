module Graphics.Seangine.VulkanFrame (withVulkanFrame) where

import Graphics.Seangine.Domain (Frame(..))
import Graphics.Seangine.Internal.GraphicsPipelineDetails
import Graphics.Seangine.Internal.SwapchainDetails
import Graphics.Seangine.Internal.Utils (throwIfUnsuccessful)
import Graphics.Seangine.Monad
import Graphics.Seangine.Window

import Control.Monad.Trans.Resource (allocate)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR())
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Zero (Zero(..))

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Bits
import Data.Traversable (for)
import GHC.Clock (getMonotonicTime)
import qualified Data.Vector as V

withVulkanFrame
  :: WindowSystem system
  => Window system window
  -> SurfaceKHR
  -> Vulkan Frame
withVulkanFrame window surface = do
  device <- getDevice
  handles <- Vulkan ask
  allocator <- getAllocator

  start <- liftIO getMonotonicTime
  swapchainDetails@SwapchainDetails{..} <- withSwapchainDetails window surface
  imageViews <- withImageViews swapchainDetails
  GraphicsPipelineDetails{..} <- withGraphicsPipelineDetails swapchainDetails
  
  return Frame
    { fIndex = 0,
      fStartTime = start,
      fSurface = surface,
      fSwapchain = sdSwapchain,
      fImageExtent = sdExtent,
      fRenderPass = renderPass,
      fFramebuffers = undefined,
      fPipelineLayout = pipelineLayout,
      fGraphicsPipeline = graphicsPipeline,
      fImageAvailable = undefined,
      fRenderFinished = undefined,
      fVertexBuffer = undefined,
      fIndexBuffer = undefined,
      fUniformBuffers = undefined,
      fDescriptorSets = undefined,
      fResources = undefined,
      fGpuWork = undefined
    }

withImageViews :: SwapchainDetails -> Vulkan (V.Vector ImageView)
withImageViews SwapchainDetails{..} = do
  device <- getDevice

  (res, images) <- getSwapchainImagesKHR device sdSwapchain
  throwIfUnsuccessful "Unable to get swapchain images" res
  
  for images $ \image ->
    withImageView' image sdSurfaceFormat IMAGE_ASPECT_COLOR_BIT
