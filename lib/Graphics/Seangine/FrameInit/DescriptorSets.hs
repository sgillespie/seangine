module Graphics.Seangine.FrameInit.DescriptorSets
  (DescriptorSetLayouts(..),
   withDescriptorSetLayouts',
   withDescriptorSets',
   maxObjects
  ) where

import Graphics.Seangine.Monad (MonadInstance(..), SeangineInstance)
import Graphics.Seangine.Render.PushConstantObject (PushConstantObject(..))
import Graphics.Seangine.Render.UniformBufferObject (UniformBufferObject(..))

import Control.Monad.Trans.Resource (allocate)
import Data.Word (Word32())
import Foreign.Storable (Storable(..))
import Prelude
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Core10
import Vulkan.Zero (Zero(..))
import qualified Data.Vector as V

maxObjects :: Int
maxObjects = 10000

data DescriptorSetLayouts = DescriptorSetLayouts
  { uniformBufferSetLayout :: DescriptorSetLayout,
    objectBufferSetLayout :: DescriptorSetLayout
  }

withDescriptorSetLayouts' :: SeangineInstance DescriptorSetLayouts
withDescriptorSetLayouts' = do
  device <- getDevice
  let uniformLayoutCreateInfo = zero
        { bindings = [uniformLayoutBinding] }

      uniformLayoutBinding = zero
        { binding = 0,
          descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER,
          descriptorCount = 1,
          stageFlags = SHADER_STAGE_VERTEX_BIT
        }

      objectLayoutCreateInfo = zero
        { bindings = [objectLayoutBinding] }

      objectLayoutBinding = zero
        { binding = 0,
          descriptorType = DESCRIPTOR_TYPE_STORAGE_BUFFER,
          descriptorCount = 1,
          stageFlags = SHADER_STAGE_VERTEX_BIT
        }

  DescriptorSetLayouts
    <$> withDescriptorSetLayout' uniformLayoutCreateInfo
    <*> withDescriptorSetLayout' objectLayoutCreateInfo

withDescriptorSets'
  :: Buffer
  -> Buffer
  -> DescriptorSetLayouts
  -> SeangineInstance (V.Vector DescriptorSet)
withDescriptorSets' uniformBuffer objectBuffer DescriptorSetLayouts{..} = do
  device <- getDevice

  descriptorPool <- withDescriptorPool'
  uniformDescriptorSets <- allocateDescriptorSets' descriptorPool uniformBufferSetLayout
  objectDescriptorSets <- allocateDescriptorSets' descriptorPool objectBufferSetLayout

  updateUniformDescriptorSets' uniformBuffer uniformDescriptorSets
  updateObjectDescriptorSets' objectBuffer objectDescriptorSets
  
  return $ uniformDescriptorSets V.++ objectDescriptorSets

withDescriptorSetLayout'
  :: DescriptorSetLayoutCreateInfo '[]
  -> SeangineInstance DescriptorSetLayout
withDescriptorSetLayout' createInfo = do
  device <- getDevice
  (_, layout) <- withDescriptorSetLayout device createInfo Nothing allocate

  return layout

withDescriptorPool' :: SeangineInstance DescriptorPool
withDescriptorPool' = do
  device <- getDevice

  let descriptorPoolCreateInfo = zero
        { maxSets = 10,
          poolSizes = [uniformPoolSize, objectPoolSize]
        }

      uniformPoolSize = DescriptorPoolSize
        { type' = DESCRIPTOR_TYPE_UNIFORM_BUFFER,
          descriptorCount = 10
        }

      objectPoolSize = DescriptorPoolSize
        { type' = DESCRIPTOR_TYPE_STORAGE_BUFFER,
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

updateUniformDescriptorSets'
  :: Buffer
  -> V.Vector DescriptorSet
  -> SeangineInstance ()
updateUniformDescriptorSets' uniformBuffer descriptorSets = do
  device <- getDevice
  V.mapM_ (updateUniformDescriptorSet' uniformBuffer) descriptorSets

updateObjectDescriptorSets' objectBuffer descriptorSets = do
  device <- getDevice
  V.mapM_ (updateObjectDescriptorSet' objectBuffer) descriptorSets

updateUniformDescriptorSet'
  :: Buffer
  -> DescriptorSet
  -> SeangineInstance ()
updateUniformDescriptorSet' uniformBuffer descriptorSet = do
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

updateObjectDescriptorSet'
  :: Buffer
  -> DescriptorSet
  -> SeangineInstance ()
updateObjectDescriptorSet' objectBuffer descriptorSet = do
  device <- getDevice
  
  let uniformDescriptorWrite = SomeStruct $ zero
        { dstSet = descriptorSet,
          dstBinding = 0,
          descriptorCount = 1,
          descriptorType = DESCRIPTOR_TYPE_STORAGE_BUFFER,
          bufferInfo = [objectDescriptorBufferInfo]
        }

      objectDescriptorBufferInfo :: DescriptorBufferInfo
      objectDescriptorBufferInfo = zero
        { buffer = objectBuffer,
          offset = 0,
          range = fromIntegral bufferSize
        }

      bufferSize = maxObjects * sizeOf (zero :: PushConstantObject)

  updateDescriptorSets device [uniformDescriptorWrite] []
