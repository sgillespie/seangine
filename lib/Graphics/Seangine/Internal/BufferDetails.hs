module Graphics.Seangine.Internal.BufferDetails
  ( BufferDetails(..),
    withBufferDetails,
    withDeviceLocalBuffer,
    copyBuffer,
    copyBufferToImage,
    pokeArrayToBuffer,
    withOneTimeCommands
  ) where

import Graphics.Seangine.Monad

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Resource
import Data.Bits ((.|.))
import Data.Word (Word32())
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Core10
import Vulkan.Zero
import VulkanMemoryAllocator (Allocation(), Allocator())
import qualified Data.Vector as V
import qualified VulkanMemoryAllocator as VMA

data BufferDetails = BufferDetails
  { bdBuffer :: Buffer,
    bdReleaseKey :: ReleaseKey,
    bdAllocation :: Allocation
  }

withBufferDetails
  :: DeviceSize
  -> BufferUsageFlags
  -> MemoryPropertyFlags
  -> Vulkan BufferDetails
withBufferDetails bufferSize bufferUsageFlags requiredMemoryFlags = do
  allocator <- getAllocator
  
  let bufferCreateInfo = zero
        { size = bufferSize,
          usage = bufferUsageFlags,
          sharingMode = SHARING_MODE_EXCLUSIVE
        }
      allocationCreateInfo = zero
        { VMA.requiredFlags = requiredMemoryFlags }

  (releaseKey, bufferCreateResult) <-
    VMA.withBuffer allocator bufferCreateInfo allocationCreateInfo allocate
  let (buffer, allocation, _) = bufferCreateResult

  return $ BufferDetails buffer releaseKey allocation

-- Create a device-local buffer and copy data to it through a staging buffer
withDeviceLocalBuffer
  :: Storable storable
  => DeviceSize
  -> BufferUsageFlags
  -> [storable]
  -> Vulkan BufferDetails
withDeviceLocalBuffer bufferSize bufferUsageFlags data' = do
  allocator <- getAllocator

  let stageUsage = BUFFER_USAGE_TRANSFER_SRC_BIT
      stageFlags = MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT
      indexUsage = BUFFER_USAGE_TRANSFER_DST_BIT .|. bufferUsageFlags
      indexFlags = MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  
  -- Create a staging buffer
  stagingBuffer <- withBufferDetails bufferSize stageUsage stageFlags
  pokeArrayToBuffer stagingBuffer data'

  -- Copy over to the vertex buffer
  buffer <- withBufferDetails bufferSize indexUsage indexFlags
  copyBuffer stagingBuffer buffer bufferSize

  return buffer

  
copyBuffer
  :: BufferDetails
  -> BufferDetails
  -> DeviceSize
  -> Vulkan ()
copyBuffer srcBuffer dstBuffer deviceSize = withOneTimeCommands $ do
  commandBuffer <- getCommandBuffer

  let bufferCopy :: BufferCopy
      bufferCopy = zero { size = deviceSize }

      BufferDetails { bdBuffer = src } = srcBuffer
      BufferDetails { bdBuffer = dest } = dstBuffer

  cmdCopyBuffer commandBuffer src dest [bufferCopy]

copyBufferToImage
  :: Buffer
  -> Image
  -> Word32
  -> Word32
  -> Vulkan ()
copyBufferToImage buffer image width height = withOneTimeCommands $ do
  commandBuffer <- getCommandBuffer

  let imageLayout = IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL

      bufferImageCopy = zero
        { imageSubresource = imageSubresource,
          imageExtent = Extent3D width height 1
        }

      imageSubresource :: ImageSubresourceLayers
      imageSubresource = zero
        { aspectMask = IMAGE_ASPECT_COLOR_BIT,
          layerCount = 1
        }

  cmdCopyBufferToImage
    commandBuffer
    buffer
    image
    imageLayout
    [bufferImageCopy]

pokeArrayToBuffer
  :: Storable storable
  => BufferDetails
  -> [storable]
  -> Vulkan ()
pokeArrayToBuffer BufferDetails{..} data' = do
  allocator <- getAllocator
  
  (releaseStagingMemory, stagingMemory) <-
    VMA.withMappedMemory allocator bdAllocation allocate
  liftIO $ pokeArray (castPtr stagingMemory) data'
  release releaseStagingMemory

withOneTimeCommands :: CmdT Vulkan a -> Vulkan a
withOneTimeCommands commands = do
  device <- getDevice
  graphicsQueue <- getGraphicsQueue

  (releaseCommandBuffer, commandBuffer) <- withOneTimeCommandBuffer
  result <- recordOneTimeCommandBuffer commandBuffer commands
  submitOneTimeCommandBuffer commandBuffer

  queueWaitIdle graphicsQueue
  release releaseCommandBuffer
  return result

withOneTimeCommandBuffer :: Vulkan (ReleaseKey, CommandBuffer)
withOneTimeCommandBuffer = do
  commandPool <- getCommandPool
  device <- getDevice

  let commandBufferInfo = zero
        { level = COMMAND_BUFFER_LEVEL_PRIMARY,
          commandBufferCount = 1,
          commandPool = commandPool
        }
  
  (releaseKey, commandBuffers) <- withCommandBuffers device commandBufferInfo allocate

  return (releaseKey, V.head commandBuffers)
  

recordOneTimeCommandBuffer
  :: CommandBuffer
  -> CmdT Vulkan a
  -> Vulkan a
recordOneTimeCommandBuffer commandBuffer commands
  = runCmdT commandBuffer beginInfo commands
  where beginInfo :: CommandBufferBeginInfo '[]
        beginInfo = zero
          { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT }

submitOneTimeCommandBuffer
  :: (MonadVulkan vulkan, MonadIO vulkan)
  => CommandBuffer
  -> vulkan ()
submitOneTimeCommandBuffer commandBuffer = do
  graphicsQueue <- getGraphicsQueue

  let submitInfo = zero
        { commandBuffers = [commandBufferHandle commandBuffer] }
  
  queueSubmit graphicsQueue [SomeStruct submitInfo] zero
