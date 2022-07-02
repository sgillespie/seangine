module Graphics.Seangine.Domain.Vertex
 (Vertex(..)) where

import Foreign.Ptr
import Foreign.Storable (Storable(..))
import Linear (V2(..), V3(..))
import Vulkan.Core10 hiding (alignment)
import Vulkan.Zero (Zero(..))

data Vertex = Vertex
  { position :: V3 Float,
    color :: V3 Float,
    textureCoordinate :: V2 Float
  }

instance Zero Vertex where
  zero = Vertex zero zero zero

instance Zero zero => Zero (V2 zero) where
  zero = V2 zero zero

instance Zero zero => Zero (V3 zero) where
  zero = V3 zero zero zero

instance Storable Vertex where
  sizeOf _
    = sizeOf (zero :: V3 Float)
      + sizeOf (zero :: V3 Float)
      + sizeOf (zero :: V2 Float)

  alignment _ = alignment (zero :: Float)

  peek ptr = Vertex <$> peek v3Ptr <*> peek v3Ptr <*> peek v2Ptr
    where v3Ptr = castPtr ptr
          v2Ptr = castPtr ptr

  poke ptr (Vertex position color textureCoordinate)
    = poke ptr' position
    >> pokeByteOff ptr' (calculateOffset [position]) color
    >> pokeByteOff ptr' (calculateOffset [position, color]) textureCoordinate
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
         offset = fromIntegral $ calculateOffset [dummyV3]
       },

      VertexInputAttributeDescription
       { location = 2,
         binding = 0,
         format = FORMAT_R32G32_SFLOAT,
         offset = fromIntegral $ calculateOffset [dummyV3, dummyV3]
       }
    ]

  where formatV3 = FORMAT_R32G32B32_SFLOAT
        formatv2 = FORMAT_R32G32_SFLOAT
        dummyV3 = zero :: V3 Float

calculateOffset values = sum $ map sizeOf values
