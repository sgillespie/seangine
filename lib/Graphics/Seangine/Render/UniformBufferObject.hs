module Graphics.Seangine.Render.UniformBufferObject
  ( UniformBufferObject(..)
  ) where

import Foreign.Ptr
import Foreign.Storable
import Linear hiding (zero)
import Prelude
import Vulkan.Zero (Zero(..))

data UniformBufferObject = UniformBufferObject
  { model :: M44 Float,
    view :: M44 Float,
    projection :: M44 Float
  }

newtype ColumnMajorM44 = ColumnMajorM44 { unM44 :: V4 (V4 Float) }

instance Storable UniformBufferObject where
  sizeOf _ = sizeOf (zero :: M44 Float) * 3

  alignment _ = alignment (zero :: Float)
  
  peek ptr = UniformBufferObject <$> peek m44Ptr <*> peek m44Ptr <*> peek m44Ptr
    where m44Ptr = castPtr ptr

  poke ptr (UniformBufferObject model' view' projection')
    = pokeElemOff m44Ptr 0 (ColumnMajorM44 model')
      >> pokeElemOff m44Ptr 1 (ColumnMajorM44 view')
      >> pokeElemOff m44Ptr 2 (ColumnMajorM44 projection')
    where m44Ptr = castPtr ptr

instance Zero UniformBufferObject where
  zero = UniformBufferObject zero zero zero

instance Storable ColumnMajorM44 where
  sizeOf _ = sizeOf $ unM44 zero
  alignment _ = sizeOf $ unM44 zero
  peek ptr = ColumnMajorM44 . transpose <$> peek (castPtr ptr)
  poke ptr = poke (castPtr ptr) . transpose . unM44

instance Zero ColumnMajorM44 where
  zero = ColumnMajorM44 $ V4 zero zero zero zero

instance Zero zero => Zero (V4 zero) where
  zero = V4 zero zero zero zero
