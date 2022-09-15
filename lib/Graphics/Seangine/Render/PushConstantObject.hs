module Graphics.Seangine.Render.PushConstantObject
  (PushConstantObject(..)) where

import Graphics.Seangine.Render.ColumnMajorM44

import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
import Linear (M44(..))
import RIO
import Vulkan.Zero (Zero(..))

newtype PushConstantObject = PushConstantObject
  { transform :: M44 Float }

instance Storable PushConstantObject where
  sizeOf _ = sizeOf (zero :: M44 Float)
  alignment _ = alignment (zero :: Float)
  peek ptr = PushConstantObject <$> peek (castPtr ptr)
  poke ptr (PushConstantObject transform)
    = poke (castPtr ptr) (ColumnMajorM44 transform)

instance Zero PushConstantObject where
  zero = PushConstantObject zero
