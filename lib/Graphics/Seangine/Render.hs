module Graphics.Seangine.Render
  (renderFrame) where

import Graphics.Seangine.Monad
import Graphics.Seangine.Render.PushConstantObject (PushConstantObject(..))
import Graphics.Seangine.Render.UniformBufferObject
import Graphics.Seangine.Render.Vertex
import Graphics.Seangine.Scene
import Graphics.Seangine.Shared.Utils (oneSecond, throwIfUnsuccessful)

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Reader (ask)
import Data.Maybe
import Data.Word
import Foreign.Marshal (pokeArray)
import Foreign.Storable
import Foreign.Ptr
import GHC.Clock (getMonotonicTime)
import Linear hiding (zero)
import Lens.Micro
import Prelude
import RIO.Vector.Boxed.Partial ((!))
import Text.GLTF.Loader
import UnliftIO.Foreign (allocaBytes, poke)
import Vulkan.Core10 hiding (withMappedMemory)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Zero (Zero(..))
import VulkanMemoryAllocator (withMappedMemory)
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as V


renderFrame :: SeangineFrame ()
renderFrame = do
  device <- getDevice
  graphicsQueue <- getGraphicsQueue
  presentQueue <- getPresentQueue
  frame@Frame{..} <- getFrame

  FrameInFlight{..} <- getFrameInFlight

  fenceResult <- waitForFencesSafe device [ffGpuWork] True oneSecond
  throwIfUnsuccessful "Timed out waiting for fence" fenceResult

  resetFences device [ffGpuWork]
  resetCommandBuffer ffCommandBuffer zero

  (imageResult, imageIndex) <- acquireNextImageKHR device fSwapchain maxBound ffImageAvailable zero
  throwIfUnsuccessful "Failed to acquire swapchain image!" imageResult

  runCmdT ffCommandBuffer zero $ recordCommandBuffer frame (fromIntegral imageIndex)

  updateUniformBuffer
  updateObjectBuffer

  let submitInfo = SomeStruct $ zero
        { waitSemaphores = [ffImageAvailable],
          waitDstStageMask = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT],
          commandBuffers = [commandBufferHandle'],
          signalSemaphores = [ffRenderFinished]
        }

      commandBufferHandle' = commandBufferHandle ffCommandBuffer

      presentInfo = zero
        { waitSemaphores = [ffRenderFinished],
          swapchains = [fSwapchain],
          imageIndices = [imageIndex]
        }
  
  queueSubmit graphicsQueue [submitInfo] ffGpuWork
  result <- queuePresentKHR presentQueue presentInfo
  throwIfUnsuccessful "Failed to present swapchain image!" result

  return ()

recordCommandBuffer :: Frame -> Int -> CmdT SeangineFrame ()
recordCommandBuffer Frame{..} imageIndex = do
  let FrameInFlight{..}
        = fFramesInFlight ! (fromIntegral fIndex `mod` fMaxFramesInFlight)
      framebuffer = fFramebuffers ! imageIndex
      commandBuffer = ffCommandBuffer

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

    cmdBindDescriptorSets
      commandBuffer
      PIPELINE_BIND_POINT_GRAPHICS
      fPipelineLayout
      0
      [ffDescriptorSets V.! 0]
      []

    cmdBindDescriptorSets
      commandBuffer
      PIPELINE_BIND_POINT_GRAPHICS
      fPipelineLayout
      1
      [ffDescriptorSets V.! 1]
      []

    V.iforM_ (fScene ^. _nodes) $ \ nodeId node -> do
      forM_ (node ^. _nodeMesh fScene) $ \(meshId, mesh) -> do
        V.iforM_ (mesh ^. _meshPrimitives) $ \id primitive' -> do
          let bufferId = MeshPrimitiveId meshId id
              indices = primitive' ^. _meshPrimitiveIndices
              vertexBuffer = fVertexBuffers Map.! bufferId
              indexBuffer = fIndexBuffers Map.! bufferId
              pushConstantSize = sizeOf (undefined :: PushConstantObject)
          
          cmdBindVertexBuffers commandBuffer 0 [vertexBuffer] [0]
          cmdBindIndexBuffer commandBuffer indexBuffer 0 INDEX_TYPE_UINT16

          cmdDrawIndexed
            commandBuffer
            (fromIntegral $ length indices)
            1
            0
            0
            (fromIntegral nodeId)

updateUniformBuffer :: SeangineFrame ()
updateUniformBuffer = do
  allocator <- getAllocator
  Frame{..} <- getFrame
  FrameInFlight{..} <- getFrameInFlight
  currentTime <- liftIO getMonotonicTime

  let elapsed = currentTime - fStartTime

      (Extent2D w h) = fImageExtent

      proj = perspective (pi/4) (fromIntegral w / fromIntegral h) 0.1 10
      (V4 pw px py pz) = proj

      rotationAngle = realToFrac $ (elapsed / 4) * (pi / 2) :: Float
      rotationMatrix
        = V3
            (V3 (cos rotationAngle) (-sin rotationAngle) 0)
            (V3 (sin rotationAngle) (cos rotationAngle) 0)
            (V3 0 0 1)

      uniformObject = UniformBufferObject
        { model = mkTransformationMat rotationMatrix zero,
          view = lookAt (V3 2 2 2) (V3 0 0 0) (V3 0 0 1),
          projection = V4 pw (-px) py pz
        }

      (_, bufferAlloc) = ffUniformBuffer

  (unmapMem, data') <- withMappedMemory allocator bufferAlloc allocate
  liftIO $ poke (castPtr data') uniformObject
  release unmapMem

  return ()

updateObjectBuffer :: SeangineFrame ()
updateObjectBuffer = do
  allocator <- getAllocator
  Frame{..} <- getFrame
  FrameInFlight{..} <- getFrameInFlight

  let (_, bufferAlloc) = ffObjectBuffer
      pushConstants = flip V.map (fScene ^. _nodes) $ \node -> 
        let rotation = fromMaybe zero (node ^. _nodeRotation . to toQuaternion)
            translation = fromMaybe zero (node ^. _nodeTranslation)
            scale = fromMaybe (V3 1 1 1) $ node ^. _nodeScale
            transMatrix = mkTransformation rotation translation
            scaleMatrix = m33_to_m44 $ scaled scale
            pushConstant = PushConstantObject $ transMatrix !*! scaleMatrix
        in pushConstant

  (unmapMem, data') <- withMappedMemory allocator bufferAlloc allocate
  liftIO $ pokeArray (castPtr data') (V.toList pushConstants)
  release unmapMem

  return ()

toQuaternion (Just (V4 w x y z)) = Just $ Quaternion w (V3 x y z)
toQuaternion _ = Nothing
