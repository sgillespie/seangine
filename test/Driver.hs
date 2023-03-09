module Main (main) where

import qualified Graphics.Seangine.AppSpec as AppSpec
import qualified Graphics.Seangine.LibSpec as LibSpec
import qualified Spec

import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Hspec

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = do
  specs' <- specs
  pure $ testGroup "Tests" [specs', properties]

specs :: IO TestTree
specs = testGroup "(checked by Hspec)" <$> specs'
  where
    specs' :: IO [TestTree]
    specs' = do
      prelude <- testSpecs Spec.spec_prelude
      appSpec <- testSpecs AppSpec.spec
      pure $ prelude ++ appSpec

properties :: TestTree
properties =
  testGroup
    "(checked by Hedgehog)"
    [testProperty "projectName is projectName" LibSpec.spec]
