module Graphics.Seangine.Frame.DescriptorSets
  (withDescriptorSets') where

import Graphics.Seangine.Frame.UniformBufferObject (UniformBufferObject(..))
import Graphics.Seangine.Monad (MonadInstance(..), SeangineInstance)

import Control.Monad.Trans.Resource (allocate)
import Data.Word (Word32())
import Foreign.Storable (Storable(..))
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Core10
import Vulkan.Zero (Zero(..))
import qualified Data.Vector as V

withDescriptorSets'
  :: V.Vector ImageView
  -> V.Vector Buffer
  -> DescriptorSetLayout
  -> SeangineInstance (V.Vector DescriptorSet)
withDescriptorSets' imageViews uniformBuffers descriptorSetLayout = do
  device <- getDevice

  let count = length imageViews
      descriptorSetLayouts = V.replicate count descriptorSetLayout

  descriptorPool <- withDescriptorPool' (fromIntegral count)
  descriptorSets <- allocateDescriptorSets' descriptorPool descriptorSetLayouts
  updateDescriptorSets' descriptorSets uniformBuffers
  
  return descriptorSets

withDescriptorPool' :: Word32 -> SeangineInstance DescriptorPool
withDescriptorPool' descriptorCount = do
  device <- getDevice

  let descriptorPoolCreateInfo = zero
        { maxSets = descriptorCount,
          poolSizes = [uniformPoolSize]
        }

      uniformPoolSize = DescriptorPoolSize
        { type' = DESCRIPTOR_TYPE_UNIFORM_BUFFER,
          descriptorCount = fromIntegral descriptorCount
        }

  snd <$> withDescriptorPool device descriptorPoolCreateInfo Nothing allocate

allocateDescriptorSets'
  :: DescriptorPool
  -> V.Vector DescriptorSetLayout
  -> SeangineInstance (V.Vector DescriptorSet)
allocateDescriptorSets' descriptorPool setLayouts = do
  device <- getDevice

  let descriptorSetAllocateInfo = zero
        { descriptorPool = descriptorPool,
          setLayouts = setLayouts
        }

  allocateDescriptorSets device descriptorSetAllocateInfo

updateDescriptorSets'
  :: V.Vector DescriptorSet
  -> V.Vector Buffer
  -> SeangineInstance ()
updateDescriptorSets' descriptorSets uniformBuffers = do
  device <- getDevice

  V.zipWithM_ updateDescriptorSet' descriptorSets uniformBuffers

updateDescriptorSet'
  :: DescriptorSet
  -> Buffer
  -> SeangineInstance ()
updateDescriptorSet' descriptorSet uniformBuffer = do
  device <- getDevice
  
  let uniformDescriptorWrite = SomeStruct $ zero
        { dstSet = descriptorSet,
          dstBinding = 0,
          descriptorCount = 1,
          descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER,
          bufferInfo = [uniformDescriptorBufferInfo]
        }

      uniformDescriptorBufferInfo :: DescriptorBufferInfo
      uniformDescriptorBufferInfo = zero
        { buffer = uniformBuffer,
          offset = 0,
          range = fromIntegral $ sizeOf (zero :: UniformBufferObject)
        }

  updateDescriptorSets device [uniformDescriptorWrite] []


