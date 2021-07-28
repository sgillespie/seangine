module Seangine.Utils where

import qualified Data.Vector as V

import Control.Monad.Trans.Resource
import Vulkan.CStruct.Extends
import Vulkan.Core10
import Vulkan.Zero


copyBuffer
  :: MonadResource m
  => Device
  -> Queue
  -> CommandPool
  -> Buffer
  -> Buffer
  -> DeviceSize
  -> m ()
copyBuffer device graphicsQueue commandPool src dst bufferSize = do
  -- Create a command buffer
  let commandBufferInfo = CommandBufferAllocateInfo
        { commandPool = commandPool ,
          level = COMMAND_BUFFER_LEVEL_PRIMARY,
          commandBufferCount = 1
        }

  (releaseCommandBuffers, commandBuffers) <-
    withCommandBuffers device commandBufferInfo allocate
  let commandBuffer = V.head commandBuffers

  -- Record the command buffer
  let beginInfo = CommandBufferBeginInfo
          { next = (),
            flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
            inheritanceInfo = Nothing
          }

  useCommandBuffer commandBuffer beginInfo $ do
    let copy = BufferCopy
          { srcOffset = zero,
            dstOffset = zero,
            size = bufferSize
          }
    
    cmdCopyBuffer commandBuffer src dst [copy]

  -- Submit the command buffer
  let submitInfo = SubmitInfo
        { next = (),
          waitSemaphores = [],
          waitDstStageMask = [],
          commandBuffers = [commandBufferHandle commandBuffer],
          signalSemaphores = []
        }
  queueSubmit graphicsQueue [SomeStruct submitInfo] zero
  queueWaitIdle graphicsQueue
  release releaseCommandBuffers

oneSecond :: Num a => a
oneSecond = 1e9

unwrapM2 :: Monad m => (m a, m b) -> m (a, b)
unwrapM2 (a, b) = do
  a' <- a
  b' <- b
  return (a', b')
