module Graphics.Seangine.Render
  ( recordCommandBuffer,
    renderFrame
  ) where

import Graphics.Seangine.Monad
import Graphics.Seangine.Frame
import Graphics.Seangine.Scene
import Graphics.Seangine.Internal.Utils (oneSecond, throwIfUnsuccessful)

import Control.Monad.IO.Unlift
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Reader (ask)
import Data.Word
import Foreign.Storable
import Foreign.Ptr (castPtr)
import GHC.Clock (getMonotonicTime)
import Linear hiding (zero)
import Lens.Micro
import Text.GLTF.Loader
import Vulkan.Core10 hiding (withMappedMemory)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Zero (Zero(..))
import VulkanMemoryAllocator (withMappedMemory)
import qualified Data.Vector as V

recordCommandBuffer :: Frame -> Framebuffer -> CmdT SeangineInstance ()
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

      primitives' = fScene ^. _allMeshPrimitives
      indices = concatMap (^. _meshPrimitiveIndices . to V.toList) primitives'

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

    cmdDrawIndexed commandBuffer (fromIntegral $ length indices) 1 0 0 0

renderFrame :: V.Vector CommandBuffer -> SeangineFrame ()
renderFrame commandBuffers = do
  device <- getDevice
  graphicsQueue <- getGraphicsQueue
  presentQueue <- getPresentQueue
  Frame{..} <- getFrame

  (imageResult, imageIndex) <- acquireNextImageKHR device fSwapchain maxBound fImageAvailable zero
  throwIfUnsuccessful "Failed to acquire swapchain image!" imageResult

  updateUniformBuffer imageIndex

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


updateUniformBuffer :: Word32 -> SeangineFrame ()
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
