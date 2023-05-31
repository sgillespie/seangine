module Graphics.Seangine.Frame.FramesInFlight (withFramesInFlight) where

import Graphics.Seangine.Config.Frame (FrameInFlight (..))
import Graphics.Seangine.Frame.DescriptorSets (DescriptorSetLayouts (..))
import Graphics.Seangine.Monad

import Data.Vector (Vector ())

withFramesInFlight :: Int -> DescriptorSetLayouts -> Vulkan (Vector FrameInFlight)
withFramesInFlight = undefined
