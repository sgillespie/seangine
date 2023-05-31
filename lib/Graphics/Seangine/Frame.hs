module Graphics.Seangine.Frame
  ( withVulkanFrame,
  ) where

import Graphics.Seangine.Config.Frame
import Graphics.Seangine.Frame.DescriptorSets
import Graphics.Seangine.Frame.FramesInFlight
import Graphics.Seangine.Frame.SwapchainDetails (SwapchainDetails (..), withSwapchainDetails)
import Graphics.Seangine.GraphicsPipeline
import Graphics.Seangine.Monad
import Graphics.Seangine.Scene
import Graphics.Seangine.Window

import Control.Monad.Trans.Resource
import Data.Vector (Vector)
import UnliftIO (getMonotonicTime)
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR ())

maxFramesInFlight :: Int
maxFramesInFlight = 3

withVulkanFrame
  :: WindowSystem system
  => Window system window
  -> SurfaceKHR
  -> Scene
  -> Vulkan Frame
withVulkanFrame window surface scene = do
  swapchainDetails@SwapchainDetails {..} <- withSwapchainDetails window surface

  startTime <- getMonotonicTime

  imageViews <- withImageViews swapchainDetails
  descriptorSetLayouts <- withDescriptorSetLayouts'
  pipelineLayout <- withPipelineLayout' descriptorSetLayouts
  renderPass <- withRenderPass' sdSurfaceFormat sdDepthFormat
  graphicsPipeline <- withGraphicsPipeline' pipelineLayout renderPass sdExtent

  depthImageView <- withDepthImageView swapchainDetails
  framebuffers <- withFramebuffers imageViews depthImageView renderPass sdExtent
  vertexBuffers <- withVertexBuffers scene
  indexBuffers <- withIndexBuffers scene
  framesInFlight <- withFramesInFlight maxFramesInFlight descriptorSetLayouts
  resources <- allocate createInternalState closeInternalState

  registerCleanupDevice

  pure
    Frame
      { fIndex = 0,
        fStartTime = startTime,
        fScene = scene,
        fSurface = surface,
        fSwapchain = sdSwapchain,
        fImageExtent = sdExtent,
        fRenderPass = renderPass,
        fFramebuffers = framebuffers,
        fPipelineLayout = pipelineLayout,
        fGraphicsPipeline = graphicsPipeline,
        fVertexBuffers = vertexBuffers,
        fIndexBuffers = indexBuffers,
        fMaxFramesInFlight = maxFramesInFlight,
        fFramesInFlight = framesInFlight,
        fResources = resources
      }

withImageViews :: SwapchainDetails -> Vulkan (Vector ImageView)
withImageViews = undefined

withDepthImageView :: SwapchainDetails -> Vulkan ImageView
withDepthImageView = undefined

withFramebuffers
  :: Vector ImageView
  -> ImageView
  -> RenderPass
  -> Extent2D
  -> Vulkan (Vector Framebuffer)
withFramebuffers = undefined

withVertexBuffers :: Scene -> Vulkan (HashMap MeshPrimitiveId Buffer)
withVertexBuffers = undefined

withIndexBuffers :: Scene -> Vulkan (HashMap MeshPrimitiveId Buffer)
withIndexBuffers = undefined

registerCleanupDevice :: Vulkan ()
registerCleanupDevice = undefined
