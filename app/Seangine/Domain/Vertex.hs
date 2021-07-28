module Seangine.Domain.Vertex (Vertex(..)) where

import Foreign.Ptr
import Foreign.Storable.Generic

import Linear
import Vulkan.Zero

-- |A vertex, containing a 2D position and RGB color
data Vertex = Vertex { position :: V2 Float, color :: V3 Float }
  deriving (Eq, Show)

instance Zero Vertex where
  zero = Vertex (V2 0 0) (V3 0 0 0)

instance Storable Vertex where
  sizeOf _ = sizeOf (undefined :: V2 Float) + sizeOf (undefined :: V3 Float)
  alignment _ = alignment (undefined :: Float)

  -- TODO[sgillespie]: This is obviously wrong
  peek ptr = Vertex <$> peek ptr' <*> peek ptr'
    where ptr' = castPtr ptr

  poke ptr (Vertex pos col)
    = poke ptr' pos >> pokeByteOff ptr' (sizeOf pos) col
    where ptr' = castPtr ptr
