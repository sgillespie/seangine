module Graphics.Seangine.Window.Sdl
  ( SdlWindow (),
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

data SdlWindow = SdlWindow

type instance Window SdlWindow = SDL.Window

instance WindowSystem SdlWindow where
  initWindowSystem _ = initSdl
  destroyWindowSystem _ = SDL.quit
  createWindow _ = createSdlWindow
  destroyWindow _ = SDL.destroyWindow
  getWindowSurface _ = getSdlWindowSurface
  getDrawableSize _ = getSdlDrawableSize
  getWindowExtensions _ = getSdlWindowExtensions
  pollWindowEvents _ = pollSdlWindowEvents

initSdl :: MonadIO io => io SdlWindow
initSdl = SDL.initialize initFlags $> SdlWindow
  where
    initFlags = [SDL.InitVideo, SDL.InitEvents]

createSdlWindow :: MonadIO io => Text -> Int -> Int -> io SDL.Window
createSdlWindow title width height = SDL.createWindow title window
  where
    window =
      SDL.defaultWindow
        { SDL.windowInitialSize = SDL.V2 (fromIntegral width) (fromIntegral height),
          SDL.windowGraphicsContext = SDL.VulkanContext
        }

getSdlWindowSurface :: MonadIO io => SDL.Window -> Instance -> io SurfaceKHR
getSdlWindowSurface window inst =
  SurfaceKHR
    <$> SDL.vkCreateSurface window (castPtr $ instanceHandle inst)

getSdlDrawableSize :: MonadIO io => SDL.Window -> io (V2 Int)
getSdlDrawableSize window = do
  (V2 width height) <- SDL.vkGetDrawableSize window
  pure $ V2 (fromIntegral width) (fromIntegral height)

getSdlWindowExtensions :: MonadIO io => SDL.Window -> io (Vector ByteString)
getSdlWindowExtensions window =
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
