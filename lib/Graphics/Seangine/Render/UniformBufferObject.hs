module Graphics.Seangine.Render.UniformBufferObject
  ( UniformBufferObject(..)
  ) where

import Graphics.Seangine.Render.ColumnMajorM44

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
