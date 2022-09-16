module Graphics.Seangine.Monad.VulkanSpec where

import Control.Monad.Trans.Reader

import Data.Functor.Identity
import RIO
import Test.Hspec

import Graphics.Seangine.Monad.Instance

newtype Dummy a = Dummy { undummy :: (Identity a) }
  deriving (Functor, Applicative, Monad)

instance MonadInstance Dummy where
  getDataDir = return "getDataDir"
  getInstance = undefined
  getPhysicalDevice = undefined
  getDevice = undefined
  getAllocator = undefined
  getGraphicsQueue = undefined
  getGraphicsQueueFamily = undefined
  getPresentQueue = undefined
  getPresentQueueFamily = undefined
  getCommandPool = undefined

spec :: Spec
spec = do
  describe "Vulkan" $ do
    it "MonadVulkan reader delegates to MonadVulkan" $ do
      let dataDir = runReaderT getDataDir ()
      undummy dataDir `shouldBe` "getDataDir"
  

