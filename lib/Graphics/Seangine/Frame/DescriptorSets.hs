module Graphics.Seangine.Frame.DescriptorSets
  ( DescriptorSetLayouts (..),
    withDescriptorSetLayouts',
  ) where

import Graphics.Seangine.Monad

import Vulkan.Core10

data DescriptorSetLayouts = DescriptorSetLayouts
  { uniformBufferSetLayout :: DescriptorSetLayout,
    objectBufferSetLayout :: DescriptorSetLayout
  }

withDescriptorSetLayouts' :: Vulkan DescriptorSetLayouts
withDescriptorSetLayouts' = undefined
