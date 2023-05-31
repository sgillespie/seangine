module Graphics.Seangine.GraphicsPipeline
  ( withGraphicsPipeline',
    withPipelineLayout',
    withRenderPass',
  ) where

import Graphics.Seangine.Frame.DescriptorSets
import Graphics.Seangine.Monad

import Vulkan.Core10

withPipelineLayout' :: DescriptorSetLayouts -> Vulkan PipelineLayout
withPipelineLayout' = undefined

withRenderPass' :: Format -> Format -> Vulkan RenderPass
withRenderPass' = undefined

withGraphicsPipeline' :: PipelineLayout -> RenderPass -> Extent2D -> Vulkan Pipeline
withGraphicsPipeline' = undefined
