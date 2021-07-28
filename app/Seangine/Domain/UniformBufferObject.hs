module Seangine.Domain.UniformBufferObject (UniformBufferObject(..)) where

import Foreign.Ptr
import Foreign.Storable.Generic

import Linear hiding (zero)
import Vulkan.Zero

-- |A uniform buffer object that can be passed to Vertex shaders
-- TODO[sgillespie]: Make this a PushConstant
data UniformBufferObject
  = UniformBufferObject
      { model :: M44 Float,
        view :: M44 Float,
        projection :: M44 Float
      } deriving (Eq, Show)

instance Storable UniformBufferObject where
  sizeOf _ = sizeOf (undefined :: M44 Float) * 3
  alignment _ = alignment (undefined :: Float)

  -- TODO[sgillespie]: This is obviously wrong
  peek ptr = UniformBufferObject <$> peek ptr' <*> peek ptr' <*> peek ptr'
    where ptr' = castPtr ptr

  poke ptr (UniformBufferObject model view proj)
    = pokeElemOff ptr' 0 (M44' model)
    >> pokeElemOff ptr' 1 (M44' view)
    >> pokeElemOff ptr' 2 (M44' proj)
    where ptr' = castPtr ptr

instance Zero UniformBufferObject where
  zero = UniformBufferObject zero' zero' zero'
    where zero' = unM44 zero

-- |A wrapper for 4x4 matrix that automatically marshalls to a Column-major
-- (Left-handed) matrix, as required by GLSL shaders
newtype M44' = M44' { unM44 :: V4 (V4 Float) }

instance Storable M44' where
  sizeOf = sizeOf . unM44
  alignment = sizeOf . unM44
  peek ptr = M44' . transpose <$> peek (castPtr ptr)
  poke ptr = poke (castPtr ptr) . transpose . unM44

instance Zero M44' where
  zero = M44' $
    V4
      (V4 0 0 0 0)
      (V4 0 0 0 0)
      (V4 0 0 0 0)
      (V4 0 0 0 0)
