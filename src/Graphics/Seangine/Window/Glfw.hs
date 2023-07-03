module Graphics.Seangine.Window.Glfw
  ( GlfwWindow (),
  ) where

import Graphics.Seangine.Window.Types (Event (..), Window (), WindowSystem (..))

import Data.Vector (Vector)
import Graphics.UI.GLFW qualified as GLFW
import Linear (V2)
import Vulkan.Core10 (Instance)
import Vulkan.Extensions.VK_KHR_surface

data GlfwWindow = GlfwWindow

type instance Window GlfwWindow = GLFW.Window

instance WindowSystem GlfwWindow where
  initWindowSystem _ = initGlfw
  destroyWindowSystem _ = pass
  createWindow _ = createGlfwWindow
  destroyWindow _ = destroyGlfwWindow
  getWindowSurface _ = getGlfwWindowSurface
  getDrawableSize _ = getGlfwDrawableSize
  getWindowExtensions _ = getGlfwWindowExtensions
  pollWindowEvents _ = pollGlfwWindowEvents

initGlfw :: MonadIO io => io GlfwWindow
initGlfw = pure GlfwWindow

createGlfwWindow :: MonadIO io => Text -> Int -> Int -> io GLFW.Window
createGlfwWindow _ _ _ = undefined

destroyGlfwWindow :: MonadIO io => GLFW.Window -> io ()
destroyGlfwWindow = undefined

getGlfwWindowSurface :: MonadIO io => GLFW.Window -> Instance -> io SurfaceKHR
getGlfwWindowSurface _ _ = undefined

getGlfwDrawableSize :: MonadIO io => GLFW.Window -> io (V2 Int)
getGlfwDrawableSize _ = undefined

getGlfwWindowExtensions :: MonadIO io => GLFW.Window -> io (Vector ByteString)
getGlfwWindowExtensions _ = undefined

pollGlfwWindowEvents :: MonadIO io => io [Event]
pollGlfwWindowEvents = undefined
