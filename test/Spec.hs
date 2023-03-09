module Spec (spec_prelude) where

import Test.Hspec

spec_prelude :: Spec
spec_prelude = describe "Prelude" $ do
  it "fails" $ do
    (1 :: Int) `shouldBe` (1 :: Int)
