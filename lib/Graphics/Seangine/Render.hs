module Graphics.Seangine.Render
  ( recordCommandBuffer,
    renderFrame
  ) where

import Graphics.Seangine.Domain (Frame(..))
import Graphics.Seangine.Internal.Utils
import Graphics.Seangine.Monad
import Graphics.Seangine.VulkanFrame

import Control.Monad.Trans.Reader (ask)
import Vulkan.Core10
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Zero (Zero(..))
import qualified Data.Vector as V

recordCommandBuffer :: Frame -> Framebuffer -> CmdT Vulkan ()
recordCommandBuffer Frame{..} framebuffer = do
  commandBuffer <- getCommandBuffer

  let renderPassBeginInfo = zero
        { renderPass = fRenderPass,
          framebuffer = framebuffer,
          renderArea = Rect2D zero fImageExtent,
          clearValues = clearValues
        }

      clearValues
        = [ Color zero,
            DepthStencil $ ClearDepthStencilValue 1 0
          ]

  cmdUseRenderPass commandBuffer renderPassBeginInfo SUBPASS_CONTENTS_INLINE $ do
    cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS fGraphicsPipeline
    cmdBindVertexBuffers commandBuffer 0 [fVertexBuffer] [0]
    cmdBindIndexBuffer commandBuffer fIndexBuffer 0 INDEX_TYPE_UINT16
    cmdBindDescriptorSets
      commandBuffer
      PIPELINE_BIND_POINT_GRAPHICS
      fPipelineLayout
      0
      [fDescriptorSets 0]
      []

    cmdDrawIndexed commandBuffer (fromIntegral $ length vertexIndices) 1 0 0 0

renderFrame :: V.Vector CommandBuffer -> VulkanFrame ()
renderFrame commandBuffers = do
  device <- getDevice
  graphicsQueue <- getGraphicsQueue
  presentQueue <- getPresentQueue
  Frame{..} <- getFrame

  (imageResult, imageIndex) <- acquireNextImageKHR device fSwapchain maxBound fImageAvailable zero
  throwIfUnsuccessful "Failed to acquire swapchain image!" imageResult

  let submitInfo = SomeStruct $ zero
        { waitSemaphores = [fImageAvailable],
          waitDstStageMask = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT],
          commandBuffers = [commandBufferHandle'],
          signalSemaphores = [fRenderFinished]
        }

      commandBufferHandle' = commandBufferHandle (commandBuffers V.! fromIntegral imageIndex)

      presentInfo = zero
        { waitSemaphores = [fRenderFinished],
          swapchains = [fSwapchain],
          imageIndices = [imageIndex]
        }
  
  queueSubmit graphicsQueue [submitInfo] fGpuWork
  result <- queuePresentKHR presentQueue presentInfo

  throwIfUnsuccessful "Failed to present swapchain image!" result

  fenceResult <- waitForFencesSafe device [fGpuWork] True oneSecond
  throwIfUnsuccessful "Timed out waiting for fence" fenceResult
  resetFences device [fGpuWork]
  
  return ()
