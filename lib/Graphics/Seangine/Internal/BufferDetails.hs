module Graphics.Seangine.Internal.BufferDetails
  ( withBufferDetails,
    copyBuffer,
    copyBufferToImage,
    withOneTimeCommands
  ) where

import Graphics.Seangine.Monad

import Control.Monad.IO.Class (MonadIO())
import Control.Monad.Trans.Resource
import Data.Word (Word32())
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Core10
import Vulkan.Zero
import VulkanMemoryAllocator (Allocation(), Allocator())
import qualified Data.Vector as V
import qualified VulkanMemoryAllocator as VMA

data BufferDetails = BufferDetails
  { buffer :: Buffer,
    bufferReleaseKey :: ReleaseKey,
    bufferAllocation :: Allocation
  }

withBufferDetails
  :: DeviceSize
  -> BufferUsageFlags
  -> MemoryPropertyFlags
  -> Vulkan BufferDetails
withBufferDetails deviceSize bufferUsageFlags memoryPropertyFlags = do
  allocator <- getAllocator
  
  let bufferCreateInfo = zero
      allocationCreateInfo = zero

  (releaseKey, bufferCreateResult) <-
    VMA.withBuffer allocator bufferCreateInfo allocationCreateInfo allocate
  let (buffer, allocation, _) = bufferCreateResult

  return $ BufferDetails buffer releaseKey allocation

copyBuffer
  :: Buffer
  -> Buffer
  -> DeviceSize
  -> Vulkan ()
copyBuffer srcBuffer destBuffer deviceSize = withOneTimeCommands $ do
  commandBuffer <- getCommandBuffer

  let bufferCopy :: BufferCopy
      bufferCopy = zero { size = deviceSize }

  cmdCopyBuffer commandBuffer srcBuffer destBuffer [bufferCopy]

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

withOneTimeCommands :: CmdT Vulkan a -> Vulkan a
withOneTimeCommands commands = do
  commandPool <- getCommandPool
  device <- getDevice
  graphicsQueue <- getGraphicsQueue

  (releaseCommandBuffer, commandBuffer) <- withOneTimeCommandBuffer
  result <- recordOneTimeCommandBuffer commandBuffer commands
  submitOneTimeCommandBuffer commandBuffer

  release releaseCommandBuffer
  return result

withOneTimeCommandBuffer :: Vulkan (ReleaseKey, CommandBuffer)
withOneTimeCommandBuffer = do
  device <- getDevice

  let commandBufferInfo = zero
        { level = COMMAND_BUFFER_LEVEL_PRIMARY,
          commandBufferCount = 1
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
