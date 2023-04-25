module Graphics.Seangine.Instance.CommandBuffers
  ( withCommandPool',
    withCommandBuffer',
  ) where

import Graphics.Seangine.Config.VulkanHandles
import Graphics.Seangine.Monad.Vulkan

import Control.Monad.Trans.Resource (MonadResource (..), allocate)
import Vulkan.Core10
import Vulkan.Zero (Zero (..))

import qualified Data.Vector as Vector

withCommandPool'
  :: MonadResource m
  => Device
  -> Word32
  -> CommandPoolCreateFlagBits
  -> m CommandPool
withCommandPool' device queueFamilyIndex flags =
  snd <$> withCommandPool device createInfo Nothing allocate
  where
    createInfo = CommandPoolCreateInfo flags queueFamilyIndex

withCommandBuffer' :: CommandPool -> Vulkan CommandBuffer
withCommandBuffer' commandPool = do
  device <- getDevice

  let commandBufferAllocateInfo =
        zero
          { commandPool = commandPool,
            level = COMMAND_BUFFER_LEVEL_PRIMARY,
            commandBufferCount = 1
          }

  Vector.head . snd
    <$> withCommandBuffers device commandBufferAllocateInfo allocate
