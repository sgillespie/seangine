module Graphics.Seangine.VulkanFrame (withVulkanFrame) where

import Graphics.Seangine.Domain (Frame(..))
import Graphics.Seangine.Internal.GraphicsPipelineDetails
import Graphics.Seangine.Internal.SwapchainDetails
import Graphics.Seangine.Internal.Utils (throwIfUnsuccessful)
import Graphics.Seangine.Monad
import Graphics.Seangine.Window

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Resource (allocate)
import Data.Bits
import Data.Traversable (for)
import GHC.Clock (getMonotonicTime)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR())
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Zero (Zero(..))
import qualified VulkanMemoryAllocator as VMA
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
  depthImageView <- withDepthImageView swapchainDetails
  framebuffers <- withFramebuffers imageViews depthImageView renderPass sdExtent
  (imageAvailable, renderFinished) <- withSemaphores
  
  return Frame
    { fIndex = 0,
      fStartTime = start,
      fSurface = surface,
      fSwapchain = sdSwapchain,
      fImageExtent = sdExtent,
      fRenderPass = renderPass,
      fFramebuffers = framebuffers,
      fPipelineLayout = pipelineLayout,
      fGraphicsPipeline = graphicsPipeline,
      fImageAvailable = imageAvailable,
      fRenderFinished = renderFinished,
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

withFramebuffers
  :: V.Vector ImageView
  -> ImageView
  -> RenderPass
  -> Extent2D
  -> Vulkan (V.Vector Framebuffer)
withFramebuffers imageViews depthImageView renderPass imageExtent = do
  device <- getDevice

  let framebufferCreateInfo :: ImageView -> FramebufferCreateInfo '[]
      framebufferCreateInfo imageView = zero
        { attachments = [imageView, depthImageView],
          renderPass = renderPass,
          width = imageWidth,
          height = imageHeight,
          layers = 1
        }

      (Extent2D imageWidth imageHeight) = imageExtent

  for imageViews $ \imageView ->
    snd <$> withFramebuffer device (framebufferCreateInfo imageView) Nothing allocate

withDepthImageView :: SwapchainDetails -> Vulkan ImageView
withDepthImageView swapchain@SwapchainDetails{..} = do
  depthImage <- withDepthImage swapchain
  withImageView' depthImage sdDepthFormat IMAGE_ASPECT_DEPTH_BIT

withSemaphores :: Vulkan (Semaphore, Semaphore)
withSemaphores = do
  device <- getDevice

  let withSemaphore' createInfo = snd <$> withSemaphore device createInfo Nothing allocate
  (,) <$> withSemaphore' zero <*> withSemaphore' zero

withDepthImage :: SwapchainDetails -> Vulkan Image
withDepthImage SwapchainDetails{..} = do
  allocator <- getAllocator
  device <- getDevice
  
  let (Extent2D width height) = sdExtent
      imageCreateInfo = zero
        { format = sdDepthFormat,
          imageType = IMAGE_TYPE_2D,
          extent = Extent3D width height 1,
          mipLevels = 1,
          arrayLayers = 1,
          samples = SAMPLE_COUNT_1_BIT,
          tiling = IMAGE_TILING_OPTIMAL,
          usage = IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT,
          sharingMode = SHARING_MODE_EXCLUSIVE
        }

      allocateCreateInfo = zero
        { VMA.requiredFlags = MEMORY_PROPERTY_DEVICE_LOCAL_BIT }
  
  (_, createImageResult) <-
    VMA.withImage allocator imageCreateInfo allocateCreateInfo allocate
  let (depthImage, _, _) = createImageResult

  return depthImage
