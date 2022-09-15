module Graphics.Seangine.Render.ColumnMajorM44
  (ColumnMajorM44(..)) where

import Foreign.Ptr
import Foreign.Storable (Storable(..))
import Linear hiding (zero)
import RIO
import Vulkan.Zero (Zero(..))

newtype ColumnMajorM44 = ColumnMajorM44 { unM44 :: V4 (V4 Float) }

instance Storable ColumnMajorM44 where
  sizeOf _ = sizeOf $ unM44 zero
  alignment _ = sizeOf $ unM44 zero
  peek ptr = ColumnMajorM44 . transpose <$> peek (castPtr ptr)
  poke ptr = poke (castPtr ptr) . transpose . unM44

instance Zero ColumnMajorM44 where
  zero = ColumnMajorM44 $ V4 zero zero zero zero

instance Zero zero => Zero (V4 zero) where
  zero = V4 zero zero zero zero

instance Zero zero => Zero (Quaternion zero) where
  zero = Quaternion zero zero

instance Zero zero => Zero (V3 zero) where
  zero = V3 zero zero zero
