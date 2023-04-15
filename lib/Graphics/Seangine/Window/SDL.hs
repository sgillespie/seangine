module Graphics.Seangine.Window.SDL (sdlWindowSystem) where

import Graphics.Seangine.Window

import Data.ByteString (packCString)
import Data.Vector (Vector)
import Foreign.Ptr (castPtr)
import Linear.V2 (V2 (..))
import SDL.Video.Vulkan
import Vulkan.Core10 (Instance (..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR (..))

import qualified Data.Vector as Vector
import qualified SDL
import qualified SDL.Event as E

data SdlWindowSystem

instance WindowSystem SdlWindowSystem where
  data Window SdlWindowSystem _ = Window {unWindow :: SDL.Window}

  initWindowSystem _ = SDL.initialize ([SDL.InitEvents, SDL.InitVideo] :: [SDL.InitFlag])
  destroyWindowSystem _ = SDL.quit
  createWindow _ title width height = Window <$> createWindow' title width height
  destroyWindow = SDL.destroyWindow . unWindow
  getVulkanExtensions = windowExts . unWindow
  createVulkanSurface instance' = createVulkanSurface' instance' . unWindow
  getDrawableSize = getDrawableSize' . unWindow
  pollWindowEvents _ = pollWindowEvents'

sdlWindowSystem :: Proxy SdlWindowSystem
sdlWindowSystem = Proxy

createWindow'
  :: MonadIO m
  => String
  -> Int
  -> Int
  -> m SDL.Window
createWindow' title width height = do
  let config =
        SDL.defaultWindow
          { SDL.windowGraphicsContext = SDL.VulkanContext,
            SDL.windowInitialSize = V2 (fromIntegral width) (fromIntegral height),
            SDL.windowResizable = True,
            SDL.windowMode = SDL.Windowed
          }

  SDL.createWindow (toText title) config

windowExts :: MonadIO m => SDL.Window -> m (Vector ByteString)
windowExts win = do
  exts <- vkGetInstanceExtensions win
  exts' <- liftIO $ mapM packCString exts

  pure $ Vector.fromList exts'

createVulkanSurface' :: MonadIO m => Instance -> SDL.Window -> m SurfaceKHR
createVulkanSurface' instance' window =
  SurfaceKHR <$> vkCreateSurface window instancePtr
  where
    instancePtr = castPtr (instanceHandle instance')

getDrawableSize' :: MonadIO m => SDL.Window -> m (Int, Int)
getDrawableSize' window = do
  (V2 windowLen windowWidth) <- vkGetDrawableSize window

  -- From CInt to Int
  let windowLen' = fromIntegral windowLen
      windowWidth' = fromIntegral windowWidth

  return (windowLen', windowWidth')

pollWindowEvents' :: MonadIO m => m [WindowEvent]
pollWindowEvents' = E.pumpEvents >> mapMaybe fromSdlEvent <$> E.pollEvents

fromSdlEvent :: SDL.Event -> Maybe WindowEvent
fromSdlEvent (SDL.Event _ SDL.QuitEvent) = Just QuitEvent
fromSdlEvent _ = Nothing
