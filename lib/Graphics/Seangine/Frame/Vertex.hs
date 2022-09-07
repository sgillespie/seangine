{-# LANGUAGE ExistentialQuantification #-}
module Graphics.Seangine.Frame.Vertex
 ( Vertex(..),
   vertexAttributeDescriptions
 ) where

import Foreign.Ptr
import Foreign.Storable (Storable(..))
import Linear (V2(..), V3(..))
import Prelude
import Vulkan.Core10 hiding (alignment, vertexAttributeDescriptions)
import Vulkan.Zero (Zero(..))

data Vertex = Vertex
  { position :: V3 Float,
    color :: V3 Float,
    textureCoordinate :: V2 Float
  }
  deriving (Show)

instance Zero Vertex where
  zero = Vertex zero zero zero

instance Zero zero => Zero (V2 zero) where
  zero = V2 zero zero

instance Zero zero => Zero (V3 zero) where
  zero = V3 zero zero zero

instance Storable Vertex where
  sizeOf _ = calculateOffset [zeroV3, zeroV3, zeroV2]
  alignment _ = alignment (zero :: Float)

  peek ptr = Vertex <$> peek v3Ptr <*> peek v3Ptr <*> peek v2Ptr
    where v3Ptr = castPtr ptr
          v2Ptr = castPtr ptr

  poke ptr (Vertex position color textureCoordinate)
    = poke ptr' position
    >> pokeByteOff ptr' (calculateOffset [zeroV3]) color
    >> pokeByteOff ptr' (calculateOffset [zeroV3, zeroV3]) textureCoordinate
    where ptr' = castPtr ptr
          

vertexAttributeDescriptions :: [VertexInputAttributeDescription]
vertexAttributeDescriptions
  = [ VertexInputAttributeDescription
       { location = 0,
         binding = 0,
         format = FORMAT_R32G32B32_SFLOAT,
         offset = 0
       },

      VertexInputAttributeDescription
       { location = 1,
         binding = 0,
         format = FORMAT_R32G32B32_SFLOAT,
         offset = fromIntegral $ calculateOffset [zeroV3]
       },

      VertexInputAttributeDescription
       { location = 2,
         binding = 0,
         format = FORMAT_R32G32_SFLOAT,
         offset = fromIntegral $ calculateOffset [zeroV3, zeroV3]
       }
    ]

  where formatV3 = FORMAT_R32G32B32_SFLOAT
        formatv2 = FORMAT_R32G32_SFLOAT
        dummyV3 = zero :: V3 Float

calculateOffset :: [EStorable] -> Int
calculateOffset values = sum $ flip map values
  $ \(EStorable s) -> sizeOf s
                                                         

data EStorable = forall s. Storable s => EStorable { unEStorable :: s }

zeroV2 :: EStorable
zeroV2 = EStorable (zero :: V2 Float)

zeroV3 :: EStorable
zeroV3 = EStorable (zero :: V3 Float)
