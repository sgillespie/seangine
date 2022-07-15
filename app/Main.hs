module Main where

import Graphics.Seangine hiding (getDataDir)
import Graphics.Seangine.Window.SDL
import Paths_seangine (getDataDir)

import Control.Monad.Extra
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Data.String
import Vulkan.Core10
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Zero (Zero(..))
import qualified Data.Vector as V

main :: IO ()
main = runResourceT $ do
  dataDir <- liftIO getDataDir
  (_, win) <- withWindow sdlWindowSystem "Seangine 0.1.1.0" 800 600
  windowExts <- getVulkanExtensions win
  instance' <- withVulkanInstance windowExts
  (_, surface) <- withWindowSurface instance' win
  handles <- withInstanceHandles dataDir instance' surface

  runInstance handles $ do
    reportPhysicalDevice
    
    initialFrame <- withVulkanFrame win surface
    commandBuffers <- withCommandBuffers' initialFrame
    runCommandBuffers initialFrame commandBuffers

    flip loopM initialFrame $ \frame -> do
      windowEvents <- pollWindowEvents win
      if shouldQuit windowEvents
        then return $ Right ()
        else do
          runFrame frame (renderFrame commandBuffers)
          return $ Left (advanceFrame frame)

reportPhysicalDevice :: SeangineInstance ()
reportPhysicalDevice = do
  instance' <- getInstance
  physicalDevice <- getPhysicalDevice
  PhysicalDeviceProperties{..} <- getPhysicalDeviceProperties physicalDevice

  let deviceType' = fromString $ show deviceType
  let message = "Using device: " <> deviceName <> " (" <> deviceType' <> ")"

  submitDebugUtilsMessageEXT
    instance'
    DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT
    DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
    zero { message = message }

runCommandBuffers :: Frame -> V.Vector CommandBuffer -> SeangineInstance ()
runCommandBuffers frame@Frame{..} commandBuffers = do
  V.forM_ (V.zip commandBuffers fFramebuffers) $ \(commandBuffer, framebuffer) ->
    runCmdT commandBuffer zero $ recordCommandBuffer frame framebuffer
    
advanceFrame :: Frame -> Frame
advanceFrame frame = frame { fIndex = succ (fIndex frame) }
