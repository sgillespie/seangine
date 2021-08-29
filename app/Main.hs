module Main where

import Control.Monad.Trans.Resource
import GHC.Clock (getMonotonicTime)
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Control.Monad.IO.Unlift
import Vulkan.Core10
import Vulkan.Zero
import qualified Graphics.UI.GLFW as GLFW

import Paths_seangine
import Seangine.GraphicsPipeline
import Seangine.Init
import Seangine.Monad
    ( Frame(..),
      Vulkan,
      MonadVulkan(getPhysicalDevice),
      runVulkan,
      runCmdT,
      runFrame )
import Seangine.Render
import Seangine.Window

main :: IO ()
main = runResourceT $ do
  dataDir <- liftIO getDataDir
  (_, window) <- withWindow "Seangine 0.1.0" 800 600
  windowExts <- liftIO $
    GLFW.getRequiredInstanceExtensions >>= traverse B.packCString . V.fromList
  instance' <- withInstance' windowExts
  (_, surface) <- withWindowSurface instance' window

  handles <- withVulkanHandles dataDir instance' surface
  runVulkan handles $ do
    reportPhysicalDevice
    
    initialFrame <- withVulkanFrame surface
    commandBuffers <- withCommandBuffers' initialFrame
    runCommandBuffers initialFrame commandBuffers

    loopJust (frame window commandBuffers) initialFrame

reportPhysicalDevice :: Vulkan ()
reportPhysicalDevice = do
  physicalDevice <- getPhysicalDevice
  props <- getPhysicalDeviceProperties physicalDevice

  let PhysicalDeviceProperties{deviceType=type', deviceName=name'} = props

  liftIO . putStrLn $ "Using device: " ++ show name' ++ " (" ++ show type' ++ ")"

runCommandBuffers :: MonadUnliftIO m => Frame -> V.Vector CommandBuffer -> m ()
runCommandBuffers frame' commandBuffers = do
  let beginInfo = CommandBufferBeginInfo
          { next = (),
            flags = zero,
            inheritanceInfo = Nothing
          }
      framebuffers = fFramebuffers frame'

  V.forM_ (V.zip commandBuffers framebuffers) $ \(commandBuffer, framebuffer) ->
    runCmdT commandBuffer beginInfo $ 
        recordCommandBuffer frame' framebuffer
  
loopJust :: Monad m => (a -> m (Maybe a)) -> a -> m ()
loopJust f x = f x >>= loopJust'
  where loopJust' (Just x') = loopJust f x'
        loopJust' Nothing = return ()

frame
  :: GLFW.Window
  -> V.Vector CommandBuffer
  -> Frame
  -> Vulkan (Maybe Frame)
frame window buffers frame' = do
  shouldClose <- liftIO $ do
    GLFW.pollEvents
    GLFW.windowShouldClose window

  if shouldClose
    then liftIO $ reportFps frame' >> return Nothing
    else do
      runFrame frame' (renderFrame buffers)
      Just <$> advanceFrame frame'

reportFps :: Frame -> IO ()
reportFps Frame{..} = do
  dataDir <- getDataDir
  end <- liftIO getMonotonicTime
  let seconds = end - fStartTime
  let fps = fromIntegral fIndex / seconds
  
  putStrLn $ "Data dir: " ++ dataDir
  putStrLn $ "Processed: " ++ show fIndex ++ " Frames"
  putStrLn $ "Ran for: " ++ show seconds ++ " Secs"
  putStrLn $ "Average: " ++ show fps ++ " FPS"

advanceFrame :: MonadResource m => Frame -> m Frame
advanceFrame f = return f { fIndex = succ (fIndex f) }
