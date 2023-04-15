module Graphics.Seangine.Window
  ( WindowEvent (..),
    WindowSystem (..),
    shouldQuit,
    withWindow,
    withWindowSurface,
    withWindowSystem,
  ) where

import Control.Monad.Trans.Resource
import Data.Vector (Vector)
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR (..), destroySurfaceKHR)

data WindowEvent
  = QuitEvent
  deriving (Eq, Show)

class WindowSystem system where
  data Window system :: Type -> Type

  initWindowSystem :: MonadIO io => Proxy system -> io ()

  destroyWindowSystem :: MonadIO io => Proxy system -> io ()

  createWindow
    :: MonadIO io
    => Proxy system
    -> String
    -- ^ Window title
    -> Int
    -- ^ Window width
    -> Int
    -- ^ Window height
    -> io (Window system window)

  destroyWindow :: MonadIO io => Window system window -> io ()

  getVulkanExtensions
    :: MonadIO io
    => Window system window
    -> io (Vector ByteString)

  createVulkanSurface
    :: MonadIO io
    => Instance
    -> Window system window
    -> io SurfaceKHR

  getDrawableSize
    :: MonadIO io
    => Window system window
    -> io (Int, Int)

  pollWindowEvents
    :: MonadResource resource
    => Window system window
    -> resource [WindowEvent]

withWindowSystem
  :: (MonadResource m, WindowSystem system)
  => Proxy system
  -> m ReleaseKey
withWindowSystem sys = allocate_ (initWindowSystem sys) (destroyWindowSystem sys)

withWindow
  :: (MonadResource m, WindowSystem system)
  => Proxy system
  -> String
  -- ^ Window title
  -> Int
  -- ^ Window width
  -> Int
  -- ^ Window height
  -> m (ReleaseKey, Window system window)
withWindow system title width height =
  withWindowSystem system >> allocate createWindow' destroyWindow
  where
    createWindow' = createWindow system title width height

withWindowSurface
  :: (MonadResource m, WindowSystem system)
  => Instance
  -> Window system window
  -> m (ReleaseKey, SurfaceKHR)
withWindowSurface instance' window =
  allocate create destroy
  where
    create = createVulkanSurface instance' window
    destroy surface = destroySurfaceKHR instance' surface Nothing

shouldQuit :: [WindowEvent] -> Bool
shouldQuit = any shouldQuit'
  where
    shouldQuit' QuitEvent = True
