module Graphics.Seangine.LibSpec (spec) where

import Graphics.Seangine.Lib (projectName)

import Hedgehog

spec :: Property
spec = property $ do
  projectName === "seangine"
