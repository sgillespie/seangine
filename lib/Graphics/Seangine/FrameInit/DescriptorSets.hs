module Graphics.Seangine.FrameInit.DescriptorSets
  (withDescriptorSets') where

import Graphics.Seangine.Monad (MonadInstance(..), SeangineInstance)
import Graphics.Seangine.Render.UniformBufferObject (UniformBufferObject(..))

import Control.Monad.Trans.Resource (allocate)
import Data.Word (Word32())
import Foreign.Storable (Storable(..))
import Prelude
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Core10
import Vulkan.Zero (Zero(..))
import qualified Data.Vector as V

withDescriptorSets'
  :: Buffer
  -> DescriptorSetLayout
  -> SeangineInstance (V.Vector DescriptorSet)
withDescriptorSets' uniformBuffer descriptorSetLayout = do
  device <- getDevice

  descriptorPool <- withDescriptorPool'
  descriptorSets <- allocateDescriptorSets' descriptorPool descriptorSetLayout
  updateDescriptorSets' uniformBuffer descriptorSets
  
  return descriptorSets

withDescriptorPool' :: SeangineInstance DescriptorPool
withDescriptorPool' = do
  device <- getDevice

  let descriptorPoolCreateInfo = zero
        { maxSets = 10,
          poolSizes = [uniformPoolSize]
        }

      uniformPoolSize = DescriptorPoolSize
        { type' = DESCRIPTOR_TYPE_UNIFORM_BUFFER,
          descriptorCount = 10
        }

  snd <$> withDescriptorPool device descriptorPoolCreateInfo Nothing allocate

allocateDescriptorSets'
  :: DescriptorPool
  -> DescriptorSetLayout
  -> SeangineInstance (V.Vector DescriptorSet)
allocateDescriptorSets' descriptorPool setLayout = do
  device <- getDevice

  let descriptorSetAllocateInfo = zero
        { descriptorPool = descriptorPool,
          setLayouts = [setLayout]
        }

  allocateDescriptorSets device descriptorSetAllocateInfo

updateDescriptorSets'
  :: Buffer
  ->  V.Vector DescriptorSet
  -> SeangineInstance ()
updateDescriptorSets' uniformBuffer descriptorSets = do
  device <- getDevice
  V.mapM_ (updateDescriptorSet' uniformBuffer) descriptorSets

updateDescriptorSet'
  :: Buffer
  -> DescriptorSet
  -> SeangineInstance ()
updateDescriptorSet' uniformBuffer descriptorSet = do
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


