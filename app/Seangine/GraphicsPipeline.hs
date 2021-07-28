module Seangine.GraphicsPipeline
  ( withVulkanFrame,
    withCommandBuffers',
    UniformBufferObject(..),
    vertices,
    vertexIndices
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Bits
import Data.Traversable
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable.Generic
import GHC.Clock (getMonotonicTime)
import qualified Data.Vector as V

import Control.Monad.Trans.Resource
import Linear hiding (zero)
import Vulkan.CStruct.Extends
import Vulkan.Core10 hiding (alignment, withBuffer, withMappedMemory)
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Zero
import VulkanMemoryAllocator

import Seangine.Domain
import Seangine.Monad
import Seangine.Shaders (fragShaderCode, vertexShaderCode)
import Seangine.Utils

vertices :: [Vertex]
vertices
  = [ Vertex (V2 (-0.5) (-0.5)) (V3 1 0 0),
      Vertex (V2   0.5  (-0.5)) (V3 0 1 0),
      Vertex (V2   0.5    0.5)  (V3 0 0 1),
      Vertex (V2 (-0.5)   0.5)  (V3 1 1 1)
    ]

vertexIndices :: [CUShort]
vertexIndices = [0, 1, 2, 2, 3, 0]

withVulkanFrame :: SurfaceKHR -> Vulkan Frame
withVulkanFrame surface = do
  device <- getDevice
  handles <- Vulkan ask
  allocator <- getAllocator
  
  (swapchain, extent) <- withSwapchain handles surface
  renderPass <- withRenderPass' device
  pipelineLayout <- withPipelineLayout' device
  pipeline <- withGraphicsPipeline pipelineLayout renderPass extent
  imageViews <- withImageViews device swapchain
  framebuffers <- withFramebuffers device imageViews renderPass extent
  (available, finished) <- withSemaphores device
  resources <- allocate createInternalState closeInternalState
  start <- liftIO getMonotonicTime
  fence <- withFence' device
  vertexBuffer <- withVertexBuffer allocator
  indexBuffer <- withIndexBuffer allocator
  uniformBuffers <- withUniformBuffers imageViews allocator
  descriptorSets <- withDescriptorSets' imageViews (V.map fst uniformBuffers)
  
  return Frame
    { fIndex = 0,
      fStartTime = start,
      fSurface = surface,
      fSwapchain = swapchain,
      fImageExtent = extent,
      fRenderPass = renderPass,
      fFramebuffers = framebuffers,
      fPipelineLayout = pipelineLayout,
      fGraphicsPipeline = pipeline,
      fImageAvailable = available,
      fRenderFinished = finished,
      fVertexBuffer = vertexBuffer,
      fUniformBuffers = uniformBuffers,
      fDescriptorSets = (descriptorSets V.!) . fromIntegral,
      fIndexBuffer = indexBuffer,
      fResources = resources,
      fGpuWork = fence
    }

withCommandBuffers' :: Frame -> Vulkan (V.Vector CommandBuffer)
withCommandBuffers' Frame{..} = do
  commandPool <- getCommandPool
  device <- getDevice
  
  let commandBufferInfo = CommandBufferAllocateInfo
        { commandPool = commandPool ,
          level = COMMAND_BUFFER_LEVEL_PRIMARY,
          commandBufferCount = fromIntegral $ V.length fFramebuffers }
  (_, commandBuffers) <- withCommandBuffers device commandBufferInfo allocate


  return commandBuffers

withSwapchain
  :: MonadResource m
  => VulkanHandles
  -> SurfaceKHR
  -> m (SwapchainKHR, Extent2D)
withSwapchain VulkanHandles{..} surface = do
  SurfaceCapabilitiesKHR{..} <-
    getPhysicalDeviceSurfaceCapabilitiesKHR vhPhysicalDevice surface

  let (sharingMode, famIndices) = if vhGraphicsQueueFamily == vhPresentQueueFamily
        then (SHARING_MODE_EXCLUSIVE, [])
        else (SHARING_MODE_CONCURRENT, [vhGraphicsQueueFamily, vhPresentQueueFamily])

      (Extent2D width height) = currentExtent
      extent = if width == maxBound && height == maxBound
          then Extent2D 800 600
          else currentExtent

      createInfo = SwapchainCreateInfoKHR
          { next = (),
            flags = zero,
            surface = surface,
            minImageCount = minImageCount + 1,
            imageFormat = FORMAT_B8G8R8A8_SRGB,
            imageColorSpace = COLOR_SPACE_SRGB_NONLINEAR_KHR,
            imageExtent = extent,
            imageArrayLayers = 1,
            imageUsage = IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
            imageSharingMode = sharingMode,
            queueFamilyIndices = famIndices,
            preTransform = currentTransform,
            compositeAlpha = COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
            presentMode = PRESENT_MODE_MAILBOX_KHR,
            clipped = True,
            oldSwapchain = zero
          }
  
  (_, swapchain) <- withSwapchainKHR vhDevice createInfo Nothing allocate
  return (swapchain, extent)

withRenderPass' :: MonadResource m => Device -> m RenderPass
withRenderPass' device = snd <$> withRenderPass device renderPassInfo Nothing allocate
  where renderPassInfo = RenderPassCreateInfo
          { next = (),
            flags = zero,
            attachments = [attachment],
            subpasses = [subpass],
            dependencies = [subpassDependency]
          }

        attachment = AttachmentDescription
          { flags = zero,
            format = FORMAT_B8G8R8A8_SRGB,
            samples = SAMPLE_COUNT_1_BIT,
            loadOp = ATTACHMENT_LOAD_OP_CLEAR,
            storeOp = ATTACHMENT_STORE_OP_STORE,
            stencilLoadOp = ATTACHMENT_LOAD_OP_DONT_CARE,
            stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE,
            initialLayout = IMAGE_LAYOUT_UNDEFINED,
            finalLayout = IMAGE_LAYOUT_PRESENT_SRC_KHR
          }

        subpass = SubpassDescription
          { flags = zero,
            pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS,
            inputAttachments = [],
            colorAttachments = [colorAttachment],
            resolveAttachments = [],
            depthStencilAttachment = Nothing,
            preserveAttachments = []
          }

        subpassDependency = SubpassDependency
          { srcSubpass = SUBPASS_EXTERNAL,
            dstSubpass = 0,
            srcStageMask = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
            srcAccessMask = zero,
            dstStageMask = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
            dstAccessMask = ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
            dependencyFlags = zero
          }

        colorAttachment = AttachmentReference
          { attachment = 0,
            layout = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
          }

withGraphicsPipeline :: PipelineLayout -> RenderPass -> Extent2D -> Vulkan Pipeline
withGraphicsPipeline pipelineLayout renderPass extent@(Extent2D width height) = do
  device <- getDevice
  
  (fragShaderModule, vertShaderModule) <- withShaderModules device
  
  let pipelineCache = zero
  
      pipelineInfo = GraphicsPipelineCreateInfo
        { next = (),
          flags = zero,
          stages = [vertexShaderStage, fragShaderStage],
          vertexInputState = Just $ SomeStruct vertexInput,
          inputAssemblyState = Just vertexAssemblyInfo,
          tessellationState = Nothing,
          viewportState = Just $ SomeStruct viewportStateInfo,
          rasterizationState = SomeStruct rasterizerInfo,
          multisampleState = Just $ SomeStruct multisampleInfo,
          depthStencilState = Nothing,
          colorBlendState = Just $ SomeStruct colorBlendInfo,
          dynamicState = Nothing,
          layout = pipelineLayout,
          renderPass = renderPass,
          subpass = 0,
          basePipelineHandle = zero,
          basePipelineIndex = zero
        }

      fragShaderStage = SomeStruct $
        PipelineShaderStageCreateInfo
          { next = (),
            flags = zero,
            stage = SHADER_STAGE_FRAGMENT_BIT,
            module' = fragShaderModule,
            name = "main",
            specializationInfo = Nothing
          }
  
      vertexShaderStage = SomeStruct $
        PipelineShaderStageCreateInfo
          { next = (),
            flags = zero,
            stage = SHADER_STAGE_VERTEX_BIT,
            module' = vertShaderModule,
            name = "main",
            specializationInfo = Nothing
          }

      vertexInput = PipelineVertexInputStateCreateInfo
        { next = (),
          flags = zero,
          vertexBindingDescriptions = [vertexBinding],
          vertexAttributeDescriptions =
            [ attributeDescriptionPos,
              attributeDescriptionColor
            ]
        }

      vertexAssemblyInfo = PipelineInputAssemblyStateCreateInfo
        { flags = zero,
          topology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
          primitiveRestartEnable = False
        }

      viewportStateInfo = PipelineViewportStateCreateInfo
        { next = (),
          flags = zero,
          viewportCount = 1,
          viewports = [viewport],
          scissorCount = 1,
          scissors = [scissor]
        }

      rasterizerInfo = PipelineRasterizationStateCreateInfo
        { next = (),
          flags = zero,
          depthClampEnable = False,
          rasterizerDiscardEnable = False,
          polygonMode = POLYGON_MODE_FILL,
          cullMode = CULL_MODE_BACK_BIT,
          frontFace = FRONT_FACE_COUNTER_CLOCKWISE,
          depthBiasEnable = False,
          depthBiasConstantFactor = 0,
          depthBiasClamp = 0,
          depthBiasSlopeFactor = 0,
          lineWidth = 1
        }

      multisampleInfo = PipelineMultisampleStateCreateInfo
        { next = (),
          flags = zero,
          rasterizationSamples = SAMPLE_COUNT_1_BIT,
          sampleShadingEnable = False,
          minSampleShading = 1,
          sampleMask = [maxBound],
          alphaToCoverageEnable = False,
          alphaToOneEnable = False
        }

      colorBlendInfo = PipelineColorBlendStateCreateInfo
        { next = (),
          flags = zero,
          logicOpEnable = False,
          logicOp = zero,
          attachments = [colorBlendAttachment],
          blendConstants = (0, 0, 0, 0)
        }

      vertexBinding = VertexInputBindingDescription
        { binding = zero,
          stride = fromIntegral $ sizeOf (undefined :: Vertex),
          inputRate = VERTEX_INPUT_RATE_VERTEX
        }

      attributeDescriptionPos = VertexInputAttributeDescription
        { location = 0,
          binding = 0,
          format = FORMAT_R32G32_SFLOAT,
          offset = 0
        }

      attributeDescriptionColor = VertexInputAttributeDescription
        { location = 1,
          binding = 0,
          format = FORMAT_R32G32B32_SFLOAT,
          offset = fromIntegral $ sizeOf (position zero)
        }

      viewport = Viewport
        { x = 0,
          y = 0,
          width = realToFrac width,
          height = realToFrac height,
          minDepth = 0,
          maxDepth = 1
        }

      scissor = Rect2D
        { offset = Offset2D 0 0,
          extent = extent }

      colorBlendAttachment = PipelineColorBlendAttachmentState
        { blendEnable = False,
          srcColorBlendFactor = BLEND_FACTOR_ONE,
          dstColorBlendFactor = BLEND_FACTOR_ZERO,
          colorBlendOp = BLEND_OP_ADD,
          srcAlphaBlendFactor = BLEND_FACTOR_ONE,
          dstAlphaBlendFactor = BLEND_FACTOR_ZERO,
          alphaBlendOp = BLEND_OP_ADD,
          colorWriteMask = COLOR_COMPONENT_R_BIT
            .|. COLOR_COMPONENT_G_BIT
            .|. COLOR_COMPONENT_B_BIT
            .|. COLOR_COMPONENT_A_BIT
        }
  
  (_, pipelines) <-
    withGraphicsPipelines device pipelineCache [SomeStruct pipelineInfo] Nothing allocate

  return . V.head . snd $ pipelines

withImageViews :: MonadResource m => Device -> SwapchainKHR -> m (V.Vector ImageView)
withImageViews device swapchain = do
  let components = ComponentMapping
        { r = COMPONENT_SWIZZLE_IDENTITY,
          g = COMPONENT_SWIZZLE_IDENTITY,
          b = COMPONENT_SWIZZLE_IDENTITY,
          a = COMPONENT_SWIZZLE_IDENTITY
        }

      subresourceRange = ImageSubresourceRange
        { aspectMask = IMAGE_ASPECT_COLOR_BIT,
          baseMipLevel = 0,
          levelCount = 1,
          baseArrayLayer = 0,
          layerCount = 1
       }

      imageViewCreateInfo img = ImageViewCreateInfo
        { next = (),
          flags = zero,
          image = img,
          viewType = IMAGE_VIEW_TYPE_2D,
          format = FORMAT_B8G8R8A8_SRGB,
          components = components,
          subresourceRange = subresourceRange
        }
        
  (_, images) <- getSwapchainImagesKHR device swapchain
  for images $ \image ->
    snd <$> withImageView device (imageViewCreateInfo image) Nothing allocate

withFramebuffers
 :: MonadResource m
 => Device
 -> V.Vector ImageView
 -> RenderPass
 -> Extent2D
 -> m (V.Vector Framebuffer)
withFramebuffers device imageViews renderPass (Extent2D width height)
  = for imageViews $ \imageView ->
      snd <$> withFramebuffer device (framebufferInfo imageView) Nothing allocate
  where framebufferInfo imageView = FramebufferCreateInfo
          { next = (),
            flags = zero,
            renderPass = renderPass,
            attachments = [imageView],
            width = width,
            height = height,
            layers = 1
          }

withSemaphores :: MonadResource m => Device -> m (Semaphore, Semaphore)
withSemaphores device = unwrapM2 (withSemaphore' zero, withSemaphore' zero)
  where withSemaphore' info = snd <$> withSemaphore device info Nothing allocate

withFence' :: MonadResource m => Device -> m Fence
withFence' device = snd <$> withFence device zero Nothing allocate

withVertexBuffer :: Allocator -> Vulkan Buffer
withVertexBuffer allocator = do
  let bufferSize = sizeOf (head vertices) * length vertices

  let stagingBufferInfo = BufferCreateInfo
        { next = (),
          flags = zero,
          size = fromIntegral bufferSize,
          usage = BUFFER_USAGE_TRANSFER_SRC_BIT,
          sharingMode = SHARING_MODE_EXCLUSIVE,
          queueFamilyIndices = [] -- Ignored
        }
      stagingAllocInfo = zero
        { requiredFlags
            = MEMORY_PROPERTY_HOST_VISIBLE_BIT
            .|. MEMORY_PROPERTY_HOST_COHERENT_BIT
        }

  (_, staging) <- withBuffer allocator stagingBufferInfo stagingAllocInfo allocate
  let (stagingBuffer, stagingAlloc, _) = staging

  (unmapStaging, data') <- withMappedMemory allocator stagingAlloc allocate
  liftIO $ pokeArray (castPtr data') vertices
  release unmapStaging
  
  let bufferInfo = BufferCreateInfo
        { next = (),
          flags = zero,
          size = fromIntegral bufferSize,
          usage
            = BUFFER_USAGE_TRANSFER_DST_BIT
            .|. BUFFER_USAGE_VERTEX_BUFFER_BIT,
          sharingMode = SHARING_MODE_EXCLUSIVE,
          queueFamilyIndices = [] -- Ignored
        }

  let allocInfo = zero
        { requiredFlags = MEMORY_PROPERTY_DEVICE_LOCAL_BIT }
  
  (_, res) <- withBuffer allocator bufferInfo allocInfo allocate
  let (buffer, _, _) = res

  -- Copy data
  device <- getDevice
  graphicsQueue <- getGraphicsQueue
  commandPool <- getCommandPool
  copyBuffer device graphicsQueue commandPool stagingBuffer buffer (fromIntegral bufferSize)
  
  return buffer

withIndexBuffer :: Allocator -> Vulkan Buffer
withIndexBuffer allocator = do
  let bufferSize = sizeOf (head vertexIndices) * length vertexIndices

  let stagingBufferInfo = BufferCreateInfo
        { next = (),
          flags = zero,
          size = fromIntegral bufferSize,
          usage = BUFFER_USAGE_TRANSFER_SRC_BIT,
          sharingMode = SHARING_MODE_EXCLUSIVE,
          queueFamilyIndices = [] -- Ignored
        }
      stagingAllocInfo = zero
        { requiredFlags
            = MEMORY_PROPERTY_HOST_VISIBLE_BIT
            .|. MEMORY_PROPERTY_HOST_COHERENT_BIT
        }

  (_, staging) <- withBuffer allocator stagingBufferInfo stagingAllocInfo allocate
  let (stagingBuffer, stagingAlloc, _) = staging

  (unmapStaging, data') <- withMappedMemory allocator stagingAlloc allocate
  liftIO $ pokeArray (castPtr data') vertexIndices
  release unmapStaging

  let bufferInfo = BufferCreateInfo
        { next = (),
          flags = zero,
          size = fromIntegral bufferSize,
          usage
            = BUFFER_USAGE_TRANSFER_DST_BIT
            .|. BUFFER_USAGE_INDEX_BUFFER_BIT,
          sharingMode = SHARING_MODE_EXCLUSIVE,
          queueFamilyIndices = [] -- Ignored
        }

      allocInfo = zero
        { requiredFlags = MEMORY_PROPERTY_DEVICE_LOCAL_BIT }
  
  (_, res) <- withBuffer allocator bufferInfo allocInfo allocate
  let (buffer, _, _) = res

  -- Copy data
  device <- getDevice
  graphicsQueue <- getGraphicsQueue
  commandPool <- getCommandPool
  copyBuffer device graphicsQueue commandPool stagingBuffer buffer (fromIntegral bufferSize)
  
  return buffer

withUniformBuffers :: V.Vector ImageView -> Allocator -> Vulkan (V.Vector (Buffer, Allocation))
withUniformBuffers imageViews allocator = do
  let bufferSize = sizeOf (zero :: UniformBufferObject)

  let bufferInfo = BufferCreateInfo
        { next = (),
          flags = zero,
          size = fromIntegral bufferSize,
          usage = BUFFER_USAGE_UNIFORM_BUFFER_BIT,
          sharingMode = SHARING_MODE_EXCLUSIVE,
          queueFamilyIndices = []
        }

      allocInfo = zero
        { requiredFlags
            = MEMORY_PROPERTY_HOST_VISIBLE_BIT
            .|. MEMORY_PROPERTY_HOST_COHERENT_BIT
        }

  forM imageViews $ \_ -> do
    (_, result) <- withBuffer allocator bufferInfo allocInfo allocate
    let (buffer, alloc, _) = result
    return (buffer, alloc)

withDescriptorSets' :: V.Vector ImageView -> V.Vector Buffer -> Vulkan (V.Vector DescriptorSet)
withDescriptorSets' imageViews uniformBuffers = do
  device <- getDevice

  -- Create the descriptor set pool
  let count = V.length imageViews
      poolSize = DescriptorPoolSize
        { type' = DESCRIPTOR_TYPE_UNIFORM_BUFFER,
          descriptorCount = fromIntegral count
        }
        
      poolInfo = DescriptorPoolCreateInfo
        { next = (),
          flags = zero,
          maxSets = fromIntegral count,
          poolSizes = [poolSize]
        }

  (_, descriptorPool) <- withDescriptorPool device poolInfo Nothing allocate

  -- Create the descriptor set layouts
  let layoutBinding = DescriptorSetLayoutBinding
        { binding = 0,
          descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER,
          descriptorCount = 1,
          stageFlags = SHADER_STAGE_VERTEX_BIT,
          immutableSamplers = []
        }

      layoutInfo = DescriptorSetLayoutCreateInfo
        { next = (),
          flags = zero,
          bindings = [layoutBinding]
        }

  (_, setLayout) <- withDescriptorSetLayout device layoutInfo Nothing allocate

  -- Create the descriptor set
  let setInfo = DescriptorSetAllocateInfo
        { next = (),
          descriptorPool = descriptorPool,
          setLayouts = V.replicate count setLayout
        }

  descriptorSets <- allocateDescriptorSets device setInfo

  _ <- V.zipWithM
    (\descriptorSet buffer -> do
        let bufferInfo = DescriptorBufferInfo
              { buffer = buffer,
                offset = 0,
                range = fromIntegral $ sizeOf (zero :: UniformBufferObject)
              }

            descriptorWrite = WriteDescriptorSet
              { next = (),
                dstSet = descriptorSet,
                dstBinding = 0,
                dstArrayElement = 0,
                descriptorCount = 1,
                descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                imageInfo = [],
                bufferInfo = [bufferInfo],
                texelBufferView = []
              }

        updateDescriptorSets device [SomeStruct descriptorWrite] []
    )
    descriptorSets
    uniformBuffers

  return descriptorSets
  

withPipelineLayout'
  :: MonadResource m
  => Device
  -> m PipelineLayout
withPipelineLayout' device = do
  setLayout <- withDescriptorSetLayout' device
  let pipelineLayoutInfo = PipelineLayoutCreateInfo
        { flags = zero,
          setLayouts = [setLayout],
          pushConstantRanges = [] }
  
  (_, pipelineLayout) <- withPipelineLayout device pipelineLayoutInfo Nothing allocate
  return pipelineLayout

withShaderModules :: MonadResource m => Device -> m (ShaderModule, ShaderModule)
withShaderModules device
  = unwrapM2
      ( snd <$> withShaderModule device fragModuleInfo Nothing allocate,
        snd <$> withShaderModule device vertModuleInfo Nothing allocate
      )

  where fragModuleInfo = ShaderModuleCreateInfo
          { next = (),
            flags = zero,
            code = fragShaderCode
          }

        vertModuleInfo = ShaderModuleCreateInfo
          {
            next = (),
            flags = zero,
            code = vertexShaderCode
          }

withDescriptorSetLayout'
  :: MonadResource m
  => Device
  -> m DescriptorSetLayout
withDescriptorSetLayout' device
  = snd <$> withDescriptorSetLayout device layoutInfo Nothing allocate

  where layoutBinding = DescriptorSetLayoutBinding
          { binding = 0,
            descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER,
            descriptorCount = 1,
            stageFlags = SHADER_STAGE_VERTEX_BIT,
            immutableSamplers = []
          }

        layoutInfo = DescriptorSetLayoutCreateInfo
          { next = (),
            flags = zero,
            bindings = [layoutBinding]
          }
