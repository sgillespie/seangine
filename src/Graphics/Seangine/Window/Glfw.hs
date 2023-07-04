module Graphics.Seangine.Window.Glfw
  ( GlfwWindowSystem (),
  ) where

import Graphics.Seangine.Window.Types (Event (..), Window (), WindowSystem (..))

import Data.Vector (Vector)
import Graphics.UI.GLFW qualified as GLFW
import Linear (V2)
import Vulkan.Core10 (Instance)
import Vulkan.Extensions.VK_KHR_surface

data GlfwWindowSystem = GlfwWindowSystem

newtype instance Window GlfwWindowSystem = GlfwWindow {unWindow :: GLFW.Window}

instance WindowSystem GlfwWindowSystem where
  initWindowSystem _ = initGlfw
  destroyWindowSystem _ = pass
  createWindow _ = createGlfwWindow
  destroyWindow = destroyGlfwWindow
  getWindowSurface = getGlfwWindowSurface
  getDrawableSize = getGlfwDrawableSize
  getWindowExtensions = getGlfwWindowExtensions
  pollWindowEvents _ = pollGlfwWindowEvents

initGlfw :: MonadIO io => io GlfwWindowSystem
initGlfw = pure GlfwWindowSystem

createGlfwWindow :: MonadIO io => Text -> Int -> Int -> io (Window GlfwWindowSystem)
createGlfwWindow _ _ _ = undefined

destroyGlfwWindow :: MonadIO io => Window GlfwWindowSystem -> io ()
destroyGlfwWindow = undefined

getGlfwWindowSurface :: MonadIO io => Window GlfwWindowSystem -> Instance -> io SurfaceKHR
getGlfwWindowSurface _ _ = undefined

getGlfwDrawableSize :: MonadIO io => Window GlfwWindowSystem -> io (V2 Int)
getGlfwDrawableSize _ = undefined

getGlfwWindowExtensions :: MonadIO io => Window GlfwWindowSystem -> io (Vector ByteString)
getGlfwWindowExtensions _ = undefined

pollGlfwWindowEvents :: MonadIO io => io [Event]
pollGlfwWindowEvents = undefined
