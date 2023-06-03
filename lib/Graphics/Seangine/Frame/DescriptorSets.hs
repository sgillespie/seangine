module Graphics.Seangine.Frame.DescriptorSets
  ( DescriptorSetLayouts (..),
    withDescriptorSetLayouts',
  ) where

import Graphics.Seangine.Config.VulkanHandles
import Graphics.Seangine.Monad

import Control.Monad.Trans.Resource
import Vulkan.Core10
import Vulkan.Zero

data DescriptorSetLayouts = DescriptorSetLayouts
  { uniformBufferSetLayout :: DescriptorSetLayout,
    objectBufferSetLayout :: DescriptorSetLayout
  }

withDescriptorSetLayouts' :: Vulkan DescriptorSetLayouts
withDescriptorSetLayouts' = do
  let uniformLayoutCreateInfo =
        zero
          { bindings = [uniformLayoutBinding]
          }

      uniformLayoutBinding =
        zero
          { binding = 0,
            descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER,
            descriptorCount = 1,
            stageFlags = SHADER_STAGE_VERTEX_BIT
          }

      objectLayoutCreateInfo =
        zero
          { bindings = [objectLayoutBinding]
          }

      objectLayoutBinding =
        zero
          { binding = 0,
            descriptorType = DESCRIPTOR_TYPE_STORAGE_BUFFER,
            descriptorCount = 1,
            stageFlags = SHADER_STAGE_VERTEX_BIT
          }

  DescriptorSetLayouts
    <$> withDescriptorSetLayout' uniformLayoutCreateInfo
    <*> withDescriptorSetLayout' objectLayoutCreateInfo

withDescriptorSetLayout'
  :: DescriptorSetLayoutCreateInfo '[]
  -> Vulkan DescriptorSetLayout
withDescriptorSetLayout' createInfo = do
  device <- getDevice
  snd <$> withDescriptorSetLayout device createInfo Nothing allocate
