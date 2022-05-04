module Graphics.Seangine.Window.SDL (sdlWindowSystem) where

import Graphics.Seangine.Window

import Linear.V2 (V2(..))
import SDL.Video.Vulkan
import Vulkan.Core10 (Instance(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR(..), destroySurfaceKHR)
import qualified SDL

import Control.Monad.IO.Class (MonadIO(..))
import Data.Maybe (mapMaybe)
import Data.Text (pack)
import Foreign.Ptr (castPtr)
import qualified Data.ByteString as B
import qualified Data.Vector as V

data SdlWindowSystem = SdlWindowSystem

instance WindowSystem SdlWindowSystem where
  data Window SdlWindowSystem _ = Window { unWindow :: SDL.Window }

  initWindowSystem _ = SDL.initialize ([SDL.InitEvents, SDL.InitVideo] :: [SDL.InitFlag])
  destroyWindowSystem _ = SDL.quit
  createWindow _ title width height = Window <$> createWindow' title width height
  destroyWindow = SDL.destroyWindow . unWindow
  getVulkanExtensions = windowExts . unWindow
  createVulkanSurface instance' = createVulkanSurface' instance' . unWindow
  getDrawableSize = getDrawableSize' . unWindow
  awaitWindowEvents _ = awaitWindowEvents'

sdlWindowSystem = SdlWindowSystem

createWindow'
  :: MonadIO m
  => String
  -> Int
  -> Int
  -> m SDL.Window
createWindow' title width height = do
    let config = SDL.defaultWindow
          { SDL.windowGraphicsContext = SDL.VulkanContext,
            SDL.windowInitialSize = V2 (fromIntegral width) (fromIntegral height),
            SDL.windowResizable = True,
            SDL.windowMode = SDL.Windowed
          }

    SDL.createWindow (pack title) config

windowExts :: MonadIO m => SDL.Window -> m (V.Vector B.ByteString)
windowExts win = do
  exts <- vkGetInstanceExtensions win
  exts' <- liftIO $ mapM B.packCString exts

  return $ V.fromList exts'

createVulkanSurface' :: MonadIO m => Instance -> SDL.Window -> m SurfaceKHR
createVulkanSurface' instance' window
  = SurfaceKHR <$> vkCreateSurface window instancePtr
  where instancePtr = castPtr (instanceHandle instance')

getDrawableSize' :: MonadIO m => SDL.Window -> m (Int, Int)
getDrawableSize' window = do
  (V2 length width) <- vkGetDrawableSize window

  -- From CInt to Int
  let length' = fromIntegral length
      width' = fromIntegral width

  return (length', width')

awaitWindowEvents' :: MonadIO m => m [WindowEvent]
awaitWindowEvents' = do
  nextEvent <- SDL.waitEvent
  pendingEvents <- SDL.pollEvents

  let events = nextEvent : pendingEvents

  return $ mapMaybe fromSdlEvent events

fromSdlEvent :: SDL.Event -> Maybe WindowEvent
fromSdlEvent (SDL.Event _ SDL.QuitEvent) = Just QuitEvent
fromSdlEvent _ = Nothing
