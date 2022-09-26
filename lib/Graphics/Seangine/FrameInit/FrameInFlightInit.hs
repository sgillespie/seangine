module Graphics.Seangine.FrameInit.FrameInFlightInit
 (withFramesInFlight) where

import Graphics.Seangine.FrameInit.BufferDetails
import Graphics.Seangine.FrameInit.DescriptorSets
import Graphics.Seangine.InstanceInit.CommandBuffers
import Graphics.Seangine.Monad
import Graphics.Seangine.Render.PushConstantObject
import Graphics.Seangine.Render.UniformBufferObject

import Control.Monad.Trans.Resource (allocate)
import Foreign.Storable (Storable(..))
import Data.Bits ((.|.))
import RIO
import RIO.Vector.Boxed
import Vulkan.Core10
import Vulkan.Zero (Zero(..))
import qualified VulkanMemoryAllocator as VMA

withFramesInFlight :: Int -> DescriptorSetLayouts -> SeangineInstance (Vector FrameInFlight)
withFramesInFlight maxFramesInFlight setLayout
  = generateM maxFramesInFlight (const $ withFrameInFlight setLayout)

withFrameInFlight :: DescriptorSetLayouts -> SeangineInstance FrameInFlight
withFrameInFlight setLayouts = do
  device <- getDevice
  queueFamily <- getGraphicsQueueFamily

  let commandPoolFlags = COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
  
  (imageAvailable, renderFinished) <- withSemaphores
  uniformBuffer <- withUniformBuffer
  objectBuffer <- withObjectBuffer
  descriptorSets <-
    withDescriptorSets' (fst uniformBuffer) (fst objectBuffer) setLayouts
  commandPool <- withCommandPool' device queueFamily commandPoolFlags
  commandBuffer <- withCommandBuffer' commandPool
  gpuWork <- withFence'
  
  return $ FrameInFlight 
    { ffImageAvailable = imageAvailable,
      ffRenderFinished = renderFinished,
      ffUniformBuffer = uniformBuffer,
      ffObjectBuffer = objectBuffer,
      ffDescriptorSets = descriptorSets,
      ffCommandPool = commandPool,
      ffCommandBuffer = commandBuffer,
      ffGpuWork = gpuWork
    }

withSemaphores :: SeangineInstance (Semaphore, Semaphore)
withSemaphores = do
  device <- getDevice

  let withSemaphore' createInfo = snd <$> withSemaphore device createInfo Nothing allocate

  imageAvailable <- withSemaphore' zero
  renderFinished <- withSemaphore' zero

  return (imageAvailable, renderFinished)

withUniformBuffer :: SeangineInstance (Buffer, VMA.Allocation)
withUniformBuffer = do
  let bufferSize = fromIntegral $ sizeOf (zero :: UniformBufferObject)
      usageFlags = BUFFER_USAGE_UNIFORM_BUFFER_BIT
      memoryFlags = MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_DEVICE_LOCAL_BIT

  BufferDetails{..} <- withBufferDetails bufferSize usageFlags memoryFlags
  return (bdBuffer, bdAllocation)

withObjectBuffer :: SeangineInstance (Buffer, VMA.Allocation)
withObjectBuffer = do
  let objectSize = sizeOf (zero :: PushConstantObject)

      bufferSize = fromIntegral $ maxObjects * objectSize
      usageFlags = BUFFER_USAGE_STORAGE_BUFFER_BIT
      memoryFlags = MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_DEVICE_LOCAL_BIT
      
  BufferDetails{..} <- withBufferDetails bufferSize usageFlags memoryFlags
  return (bdBuffer, bdAllocation)

withFence' :: SeangineInstance Fence
withFence' = do
  device <- getDevice

  let fenceCreateInfo :: FenceCreateInfo '[]
      fenceCreateInfo = zero { flags = FENCE_CREATE_SIGNALED_BIT }

  snd <$> withFence device fenceCreateInfo Nothing allocate
