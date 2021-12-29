module Graphics.Seangine.InitSpec where

import Graphics.Seangine
import Graphics.Seangine.Init

import Control.Monad.Trans.Resource
import Vulkan.Core10
import Test.Hspec

import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Foreign.Ptr

spec :: Spec
spec = do
  describe "Init" $ do
    it "withInstance" $ True `shouldBe` True

