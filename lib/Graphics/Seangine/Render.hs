module Graphics.Seangine.Render
  (recordCommandBuffer) where

import Graphics.Seangine.Domain (Frame(..))
import Graphics.Seangine.VulkanFrame
import Graphics.Seangine.Monad

import Control.Monad.Trans.Reader (ask)
import Vulkan.Core10
import Vulkan.Zero (Zero(..))

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

