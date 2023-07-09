module Graphics.Seangine.Window.Sdl
  ( SdlWindowSystem (),
  ) where

import Graphics.Seangine.Window.Types (Event (..), Window (), WindowSystem (..))

import Data.ByteString (packCString)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Foreign.Ptr (castPtr)
import Linear (V2 (..))
import SDL qualified
import SDL.Video.Vulkan qualified as SDL
import Vulkan.Core10 (Instance)
import Vulkan.Core10.Handles (instanceHandle)
import Vulkan.Extensions.VK_KHR_surface

data SdlWindowSystem = SdlWindowSystem

newtype instance Window SdlWindowSystem = SdlWindow {unWindow :: SDL.Window}

instance WindowSystem SdlWindowSystem where
  initWindowSystem _ = initSdl
  destroyWindowSystem _ = SDL.quit
  createWindow _ = createSdlWindow
  destroyWindow = SDL.destroyWindow . unWindow
  getWindowSurface = getSdlWindowSurface
  getDrawableSize = getSdlDrawableSize
  getWindowExtensions = getSdlWindowExtensions
  pollWindowEvents _ _ = pollSdlWindowEvents

initSdl :: MonadIO io => io SdlWindowSystem
initSdl = SDL.initialize initFlags $> SdlWindowSystem
  where
    initFlags = [SDL.InitVideo, SDL.InitEvents]

createSdlWindow :: MonadIO io => Text -> Int -> Int -> io (Window SdlWindowSystem)
createSdlWindow title width height = SdlWindow <$> SDL.createWindow title window
  where
    window =
      SDL.defaultWindow
        { SDL.windowInitialSize = SDL.V2 (fromIntegral width) (fromIntegral height),
          SDL.windowGraphicsContext = SDL.VulkanContext
        }

getSdlWindowSurface :: MonadIO io => Window SdlWindowSystem -> Instance -> io SurfaceKHR
getSdlWindowSurface (SdlWindow window) inst =
  SurfaceKHR
    <$> SDL.vkCreateSurface window (castPtr $ instanceHandle inst)

getSdlDrawableSize :: MonadIO io => Window SdlWindowSystem -> io (V2 Int)
getSdlDrawableSize (SdlWindow window) = do
  (V2 width height) <- SDL.vkGetDrawableSize window
  pure $ V2 (fromIntegral width) (fromIntegral height)

getSdlWindowExtensions :: MonadIO io => Window SdlWindowSystem -> io (Vector ByteString)
getSdlWindowExtensions (SdlWindow window) =
  SDL.vkGetInstanceExtensions window
    >>= liftIO . mapM packCString . Vector.fromList

pollSdlWindowEvents :: MonadIO io => io [Event]
pollSdlWindowEvents = map fromSdlEvent <$> SDL.pollEvents

fromSdlEvent :: SDL.Event -> Event
fromSdlEvent (SDL.Event _ payload) = case payload of
  SDL.QuitEvent -> QuitEvent
  (SDL.WindowResizedEvent _) -> WindowResizedEvent
  (SDL.WindowMovedEvent _) -> WindowMovedEvent
  _ -> UnknownEvent
