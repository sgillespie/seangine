module Graphics.Seangine.WindowSpec where

import Graphics.Seangine.Window

import Control.Monad.Trans.Resource
import Test.Hspec

import Control.Monad.IO.Class (MonadIO(..), liftIO)

spec :: Spec
spec = do
  describe "Window" $ do
    it "withWindow opens a vulkan window" $ do
      WindowConfig{windowGraphicsContext=ctx} <- runResourceT $ do
        (_, win) <- withWindow "WindowSpec" 800 600
        getWindowConfig win

      ctx `shouldBe` VulkanContext

    it "windowExts returns surface" $ do
      exts <- runResourceT $ do
        (_, win) <- withWindow "WindowSpec" 800 600
        windowExts win

      exts `shouldSatisfy` elem "VK_KHR_surface"
