module Graphics.Seangine.Internal.GraphicsPipelineDetails
  (GraphicsPipelineDetails(..),
   withGraphicsPipelineDetails) where

import Graphics.Seangine.Domain
import Graphics.Seangine.Internal.SwapchainDetails (SwapchainDetails(..))
import Graphics.Seangine.Monad (MonadInstance(..), SeangineInstance(..))
import Graphics.Seangine.Shaders

import Control.Monad.Trans.Resource (allocate)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Core10
import Vulkan.Zero (Zero(..))
import Data.Bits (Bits(..))
import Foreign.Storable (Storable(..))
import qualified Data.Vector as V

data GraphicsPipelineDetails = GraphicsPipelineDetails
  { descriptorSetLayout :: DescriptorSetLayout,
    pipelineLayout :: PipelineLayout,
    renderPass :: RenderPass,
    graphicsPipeline :: Pipeline
  }

withGraphicsPipelineDetails :: SwapchainDetails -> SeangineInstance GraphicsPipelineDetails
withGraphicsPipelineDetails SwapchainDetails{..} = do
  descriptorSetLayout <- withDescriptorSetLayout'
  pipelineLayout <- withPipelineLayout' descriptorSetLayout
  renderPass <- withRenderPass' sdSurfaceFormat sdDepthFormat
  graphicsPipeline <- withGraphicsPipeline pipelineLayout renderPass sdExtent

  return $ GraphicsPipelineDetails
    { descriptorSetLayout = descriptorSetLayout,
      pipelineLayout = pipelineLayout, 
      renderPass = renderPass,
      graphicsPipeline = graphicsPipeline
    }

withDescriptorSetLayout' :: SeangineInstance DescriptorSetLayout
withDescriptorSetLayout' = do
  device <- getDevice
  let createInfo = zero
        { bindings = [uniformLayoutBinding] }

      uniformLayoutBinding = zero
        { binding = 0,
          descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER,
          descriptorCount = 1,
          stageFlags = SHADER_STAGE_VERTEX_BIT
        }

  snd <$> withDescriptorSetLayout device createInfo Nothing allocate

withPipelineLayout' :: DescriptorSetLayout -> SeangineInstance PipelineLayout
withPipelineLayout' setLayout = do
  device <- getDevice
  
  let createInfo :: PipelineLayoutCreateInfo
      createInfo = zero
        { setLayouts = [setLayout] }
  
  snd <$> withPipelineLayout device createInfo Nothing allocate

withRenderPass' :: Format -> Format -> SeangineInstance RenderPass
withRenderPass' colorFormat depthFormat = do
  device <- getDevice

  let createInfo = zero
        { attachments = [colorAttachment, depthAttachment],
          subpasses = [subpass],
          dependencies = [subpassDependency]
        }

      colorAttachment = zero
        { format = colorFormat,
          samples = SAMPLE_COUNT_1_BIT,
          loadOp = ATTACHMENT_LOAD_OP_CLEAR,
          storeOp = ATTACHMENT_STORE_OP_STORE,
          stencilLoadOp = ATTACHMENT_LOAD_OP_DONT_CARE,
          stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE,
          initialLayout = IMAGE_LAYOUT_UNDEFINED,
          finalLayout = IMAGE_LAYOUT_PRESENT_SRC_KHR
        }

      depthAttachment = zero
        { format = depthFormat,
          samples = SAMPLE_COUNT_1_BIT,
          loadOp = ATTACHMENT_LOAD_OP_CLEAR,
          storeOp = ATTACHMENT_STORE_OP_STORE,
          stencilLoadOp = ATTACHMENT_LOAD_OP_DONT_CARE,
          stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE,
          initialLayout = IMAGE_LAYOUT_UNDEFINED,
          finalLayout = IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
        }

      subpass = zero
        { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS,
          colorAttachments = [colorAttachmentRef],
          depthStencilAttachment = Just depthStencilAttachment
        }

      subpassDependency = zero
        { srcSubpass = SUBPASS_EXTERNAL,
          dstSubpass = 0,
          srcStageMask
            = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
              .|. PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT,
          dstStageMask
            = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
              .|. PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT,
          dstAccessMask
            = ACCESS_COLOR_ATTACHMENT_WRITE_BIT
              .|. ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
        }

      (colorAttachmentRef, depthStencilAttachment) = subpassAttachmentRefs

  snd <$> withRenderPass device createInfo Nothing allocate

withGraphicsPipeline :: PipelineLayout -> RenderPass -> Extent2D -> SeangineInstance Pipeline
withGraphicsPipeline layout renderPass extent@(Extent2D width height) = do
  device <- getDevice

  pipelineShaderStages <- withPipelineShaderStages

  let pipelineCache = zero
      pipelineCreateInfo = SomeStruct $ zero
        { stages = pipelineShaderStages,
          vertexInputState = Just vertexInput,
          inputAssemblyState = Just inputAssembly,
          viewportState = Just viewportState,
          rasterizationState = rasterizer,
          multisampleState = Just multisample,
          depthStencilState = Just depthStencil,
          colorBlendState = Just colorBlend,
          layout = layout,
          renderPass = renderPass
        }

      vertexInput = SomeStruct $ zero
        { vertexBindingDescriptions = [vertexInputBindingDescription],
          Vulkan.Core10.vertexAttributeDescriptions = V.fromList Graphics.Seangine.Domain.vertexAttributeDescriptions
        }

      inputAssembly = zero { topology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST }

      viewportState = SomeStruct $ zero
        { viewportCount = 1,
          viewports = [pipelineViewport extent],
          scissorCount = 1,
          scissors = [pipelineScissor extent]
        }

      rasterizer = SomeStruct $ zero
        { cullMode = CULL_MODE_BACK_BIT,
          depthBiasEnable = False,
          depthClampEnable = False,
          frontFace = FRONT_FACE_COUNTER_CLOCKWISE,
          lineWidth = 1,
          polygonMode = POLYGON_MODE_FILL,
          rasterizerDiscardEnable = False
        }

      multisample = SomeStruct $ zero
        { rasterizationSamples = SAMPLE_COUNT_1_BIT,
          minSampleShading = 1
        }

      depthStencil = zero
        { depthTestEnable = True,
          depthWriteEnable = True,
          depthCompareOp = COMPARE_OP_LESS,
          minDepthBounds = 0,
          maxDepthBounds = 1
        }

      colorBlend = SomeStruct
        (zero { attachments = [pipelineColorBlendAttachment] } :: PipelineColorBlendStateCreateInfo '[])

      vertexInputBindingDescription = zero
        { stride = fromIntegral $ sizeOf (zero :: Vertex),
          inputRate = VERTEX_INPUT_RATE_VERTEX
        }

  (_, pipelines) <-
    withGraphicsPipelines device pipelineCache [pipelineCreateInfo] Nothing allocate

  return . V.head . snd $ pipelines

subpassAttachmentRefs :: (AttachmentReference, AttachmentReference)
subpassAttachmentRefs = (colorAttachmentRef, depthStencilAttachment)
  where colorAttachmentRef = zero
          { attachment = 0,
            layout = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
          }

        depthStencilAttachment = zero
          { attachment = 1,
            layout = IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
          }

withPipelineShaderStages :: SeangineInstance (V.Vector (SomeStruct PipelineShaderStageCreateInfo))
withPipelineShaderStages = do
  (vertexShaderModule, fragShaderModule) <- withShaderModules
  
  let vertexShaderStage = SomeStruct $ zero
        { stage = SHADER_STAGE_VERTEX_BIT,
          module' = vertexShaderModule,
          name = "main"
        }
        
      fragShaderStage = SomeStruct $ zero
        { stage = SHADER_STAGE_FRAGMENT_BIT,
          module' = fragShaderModule,
          name = "main"
        }
  
  return [vertexShaderStage, fragShaderStage]

pipelineViewport :: Extent2D -> Viewport
pipelineViewport extent@(Extent2D width height) = zero
  { x = 0,
    y = 0,
    width = realToFrac width,
    height = realToFrac height,
    minDepth = 0,
    maxDepth = 1
  }

pipelineScissor :: Extent2D -> Rect2D
pipelineScissor extent = Rect2D
  { offset = Offset2D 0 0,
    extent = extent
  }

pipelineColorBlendAttachment :: PipelineColorBlendAttachmentState
pipelineColorBlendAttachment = zero
  { blendEnable = False,
    srcColorBlendFactor = BLEND_FACTOR_ONE,
    dstColorBlendFactor = BLEND_FACTOR_ZERO,
    colorBlendOp = BLEND_OP_ADD,
    srcAlphaBlendFactor = BLEND_FACTOR_ONE,
    dstAlphaBlendFactor = BLEND_FACTOR_ZERO,
    alphaBlendOp = BLEND_OP_ADD,
    colorWriteMask
      = COLOR_COMPONENT_R_BIT
        .|. COLOR_COMPONENT_G_BIT
        .|. COLOR_COMPONENT_B_BIT
        .|. COLOR_COMPONENT_A_BIT
  }

withShaderModules :: SeangineInstance (ShaderModule, ShaderModule)
withShaderModules = do
  device <- getDevice

  let vertexShaderInfo = zero { code = vertexShaderCode }
      fragShaderInfo = zero { code = fragShaderCode }

  (_, vertexShaderModule) <- withShaderModule device vertexShaderInfo Nothing allocate
  (_, fragShaderModule) <- withShaderModule device fragShaderInfo Nothing allocate

  return (vertexShaderModule, fragShaderModule)
