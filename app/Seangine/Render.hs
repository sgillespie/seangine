module Seangine.Render (renderFrame, recordCommandBuffer) where

import Control.Monad.Trans.Reader
import Data.Word
import Foreign.Storable.Generic
import Foreign.Ptr (castPtr)
import GHC.Clock (getMonotonicTime)
import qualified Data.Vector as V

import Control.Monad.IO.Unlift
import Control.Monad.Trans.Resource
import Linear hiding (zero)
import Vulkan.CStruct.Extends
import Vulkan.Core10 hiding (withMappedMemory)
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Zero
import VulkanMemoryAllocator (withMappedMemory)

import Seangine.GraphicsPipeline
import Seangine.Monad
import Seangine.Utils

renderFrame :: V.Vector CommandBuffer -> VulkanFrame ()
renderFrame commandBuffers = do
  device <- getDevice
  graphicsQueue <- getGraphicsQueue
  presentQueue <- getPresentQueue
  Frame{..} <- getFrame

  (_, imageIndex) <- acquireNextImageKHR device fSwapchain maxBound fImageAvailable zero

  updateUniformBuffer imageIndex

  let submitInfo = SomeStruct $ SubmitInfo
        { next = (),
          waitSemaphores = [fImageAvailable],
          waitDstStageMask = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT],
          commandBuffers = [handle],
          signalSemaphores = [fRenderFinished]
        }
      commandBuffer = commandBuffers V.! fromIntegral imageIndex
      handle = commandBufferHandle commandBuffer
      
      presentInfo = PresentInfoKHR
        { next = (),
          waitSemaphores = [fRenderFinished],
          swapchains = [fSwapchain],
          imageIndices = [imageIndex],
          results = zero
        }

  queueSubmit graphicsQueue [submitInfo] fGpuWork
  _ <- queuePresentKHR presentQueue presentInfo

  -- Do not proceed until the previous frame has finished
  fResult <- waitForFencesSafe device [fGpuWork] True oneSecond
  case fResult of
    SUCCESS -> return ()
    err -> liftIO $ throwM $ userError (show err)
  resetFences device [fGpuWork]
  
  return ()

recordCommandBuffer :: MonadUnliftIO m => Frame -> Framebuffer -> CmdT m ()
recordCommandBuffer Frame{..} framebuffer = do
  commandBuffer <- CmdT ask

  let renderPassInfo = RenderPassBeginInfo
          { next = (),
            renderPass = fRenderPass,
            framebuffer = framebuffer,
            renderArea = Rect2D zero fImageExtent,
            clearValues = [Color $ Float32 0.1 0.1 0.1 0]
          }
  
  cmdUseRenderPass commandBuffer renderPassInfo SUBPASS_CONTENTS_INLINE $ do
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

updateUniformBuffer :: Word32 -> VulkanFrame ()
updateUniformBuffer imageIndex = do
  allocator <- getAllocator
  Frame{..} <- getFrame
  currentTime <- liftIO getMonotonicTime

  let elapsed = currentTime - fStartTime

      (Extent2D w h) = fImageExtent

      proj = perspective (pi/4) (fromIntegral w / fromIntegral h) 0.1 10
      (V4 pw px py pz) = proj

      rotationAngle = realToFrac $ elapsed * (pi / 2) :: Float
      rotationMatrix
        = V3
            (V3 (cos rotationAngle) (-sin rotationAngle) 0)
            (V3 (sin rotationAngle) (cos rotationAngle) 0)
            (V3 0 0 1)

      uniformObject = UniformBufferObject
        { model = mkTransformationMat rotationMatrix (V3 0 0 0),
          view = lookAt (V3 2 2 2) (V3 0 0 0) (V3 0 0 1),
          projection = V4 pw (-px) py pz
        }

      (_, bufferAlloc) = fUniformBuffers V.! fromIntegral imageIndex

  (unmapMem, data') <- withMappedMemory allocator bufferAlloc allocate
  liftIO $ poke (castPtr data') uniformObject
  release unmapMem

  return ()
