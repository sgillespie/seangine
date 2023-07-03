module Graphics.Seangine.Window
  ( GlfwWindow,
    SdlWindow,
    WindowSystem (..),
    glfw,
    sdl,
    withWindowSystem,
    withWindow,
    withWindowSurface,
  ) where

import Graphics.Seangine.Window.Glfw (GlfwWindow)
import Graphics.Seangine.Window.Sdl (SdlWindow)
import Graphics.Seangine.Window.Types (Window, WindowSystem (..))

import Control.Monad.Trans.Resource
import Vulkan.Core10 (Instance ())
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR (..), destroySurfaceKHR)

sdl :: Proxy SdlWindow
sdl = Proxy

glfw :: Proxy GlfwWindow
glfw = Proxy

withWindowSystem
  :: (MonadResource m, WindowSystem window)
  => Proxy window
  -> m (ReleaseKey, window)
withWindowSystem win = allocate (initWindowSystem win) destroyWindowSystem

withWindow
  :: (MonadResource m, WindowSystem window)
  => window
  -> Text
  -> Int
  -> Int
  -> m (ReleaseKey, Window window)
withWindow win title width height = do
  let create = createWindow win title width height
      destroy = destroyWindow win

  allocate create destroy

withWindowSurface
  :: (MonadResource m, WindowSystem window)
  => window
  -> Window window
  -> Instance
  -> m (ReleaseKey, SurfaceKHR)
withWindowSurface win handle inst = allocate create destroy
  where
    create = getWindowSurface win handle inst
    destroy = flip (destroySurfaceKHR inst) Nothing
