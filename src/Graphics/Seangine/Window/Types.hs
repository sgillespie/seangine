module Graphics.Seangine.Window.Types
  ( Event (..),
    Window (),
    WindowSystem (..),
  ) where

import Data.Vector (Vector ())
import Linear (V2 (..))
import Vulkan.Core10 (Instance ())
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR ())

data Event
  = QuitEvent
  | WindowResizedEvent
  | WindowMovedEvent
  | UnknownEvent
  deriving (Enum, Bounded, Eq, Show)

data family Window window

class WindowSystem system where
  initWindowSystem :: MonadIO io => Proxy system -> io system
  destroyWindowSystem :: MonadIO io => system -> io ()

  createWindow
    :: MonadIO io
    => system
    -> Text
    -> Int
    -> Int
    -> io (Window system)

  destroyWindow
    :: MonadIO io
    => Window system
    -> io ()

  getWindowSurface
    :: MonadIO io
    => Window system
    -> Instance
    -> io SurfaceKHR

  getDrawableSize
    :: MonadIO io
    => Window system
    -> io (V2 Int)

  getWindowExtensions
    :: MonadIO io
    => Window system
    -> io (Vector ByteString)

  pollWindowEvents :: MonadIO io => system -> Window system -> io [Event]
