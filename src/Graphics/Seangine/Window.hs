module Graphics.Seangine.Window
  ( GlfwWindowSystem,
    SdlWindowSystem,
    WindowSystem (..),
    withGlfw,
    withSdl,
    withWindowSystem,
    withWindow,
    withWindowSurface,
  ) where

import Graphics.Seangine.Window.Glfw (GlfwWindowSystem)
import Graphics.Seangine.Window.Sdl (SdlWindowSystem)
import Graphics.Seangine.Window.Types (Window, WindowSystem (..))

import Control.Monad.Trans.Resource
import Vulkan.Core10 (Instance ())
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR (..), destroySurfaceKHR)

withGlfw :: (MonadResource m) => m (ReleaseKey, GlfwWindowSystem)
withGlfw = withWindowSystem (Proxy :: Proxy GlfwWindowSystem)

withSdl :: (MonadResource m) => m (ReleaseKey, SdlWindowSystem)
withSdl = withWindowSystem (Proxy :: Proxy SdlWindowSystem)

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
      destroy = destroyWindow

  allocate create destroy

withWindowSurface
  :: (MonadResource m, WindowSystem window)
  => Window window
  -> Instance
  -> m (ReleaseKey, SurfaceKHR)
withWindowSurface win inst = allocate create destroy
  where
    create = getWindowSurface win inst
    destroy = flip (destroySurfaceKHR inst) Nothing
