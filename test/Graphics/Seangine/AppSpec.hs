module Graphics.Seangine.AppSpec (spec) where

import Graphics.Seangine.App

import Test.Hspec

spec :: Spec
spec = describe "run" $ do
  it "Runs reader" $ do
    let app :: App Int Text
        app = ("Got " <>) . show <$> ask

    runApp app 1 >>= (`shouldBe` "Got 1")

  it "fails" $ do
    (1 :: Int) `shouldBe` (1 :: Int)
