module Graphics.Seangine.InstanceInit.CommandBuffers
  (withCommandPool',
   withCommandBuffer'
  ) where

import Graphics.Seangine.Monad.Instance

import Control.Monad.Trans.Resource (MonadResource(..), allocate)
import RIO
import RIO.Vector.Boxed.Partial
import Vulkan.Core10
import Vulkan.Zero (Zero(..))

withCommandPool'
  :: MonadResource m
  => Device
  -> Word32
  -> CommandPoolCreateFlagBits
  -> m CommandPool
withCommandPool' device queueFamilyIndex flags
  = snd <$> withCommandPool device createInfo Nothing allocate
  where createInfo = CommandPoolCreateInfo flags queueFamilyIndex

withCommandBuffer' :: CommandPool -> SeangineInstance CommandBuffer
withCommandBuffer' commandPool = do
  device <- getDevice

  let commandBufferAllocateInfo = zero
        { commandPool = commandPool,
          level = COMMAND_BUFFER_LEVEL_PRIMARY,
          commandBufferCount = 1
        }
  
  head . snd <$> withCommandBuffers device commandBufferAllocateInfo allocate
