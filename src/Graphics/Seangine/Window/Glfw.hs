module Graphics.Seangine.Window.Glfw
  ( GlfwWindowSystem (),
  ) where

import Graphics.Seangine.Types (Errors (..))
import Graphics.Seangine.Window.Types (Event (..), Window (), WindowSystem (..))

import Control.Exception (throwIO)
import Data.ByteString (packCString)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Graphics.UI.GLFW qualified as GLFW
import Linear (V2 (..))
import Vulkan.Core10 (Instance, instanceHandle)
import Vulkan.Extensions.VK_KHR_surface

data GlfwWindowSystem = GlfwWindowSystem

newtype instance Window GlfwWindowSystem = GlfwWindow {unWindow :: GLFW.Window}

instance WindowSystem GlfwWindowSystem where
  initWindowSystem _ = initGlfw
  destroyWindowSystem _ = destroyGlfw
  createWindow _ = createGlfwWindow
  destroyWindow = destroyGlfwWindow
  getWindowSurface = getGlfwWindowSurface
  getDrawableSize = getGlfwDrawableSize
  getWindowExtensions = getGlfwWindowExtensions
  pollWindowEvents _ = pollGlfwWindowEvents

initGlfw :: MonadIO io => io GlfwWindowSystem
initGlfw = liftIO $ GLFW.init >>= bool (throwIO WindowSystemFailure) setWindowHints
  where
    setWindowHints = GLFW.windowHint (GLFW.WindowHint'Resizable False) >> pure GlfwWindowSystem

destroyGlfw :: MonadIO io => io ()
destroyGlfw = liftIO GLFW.terminate

createGlfwWindow :: MonadIO io => Text -> Int -> Int -> io (Window GlfwWindowSystem)
createGlfwWindow title width height =
  liftIO $ maybe (throwIO WindowSystemFailure) (pure . GlfwWindow) =<< createWindow'
  where
    createWindow' = GLFW.createWindow width height title' Nothing Nothing
    title' = toString title

destroyGlfwWindow :: MonadIO io => Window GlfwWindowSystem -> io ()
destroyGlfwWindow = liftIO . GLFW.destroyWindow . unWindow

getGlfwWindowSurface :: MonadIO io => Window GlfwWindowSystem -> Instance -> io SurfaceKHR
getGlfwWindowSurface (GlfwWindow window) inst =
  liftIO $ alloca @SurfaceKHR createWindowSurface'
  where
    allocPtr = nullPtr
    instPtr = castPtr $ instanceHandle inst

    createWindowSurface' surfacePtr = do
      res <- GLFW.createWindowSurface @Int instPtr window allocPtr surfacePtr
      if res == 0
        then pure $ ptrToSurfaceKhr surfacePtr
        else throwIO WindowSystemFailure

    ptrToSurfaceKhr ptr = case ptrToWordPtr ptr of
      WordPtr handle -> SurfaceKHR $ fromIntegral handle

getGlfwDrawableSize :: MonadIO io => Window GlfwWindowSystem -> io (V2 Int)
getGlfwDrawableSize = liftIO . getWindowSize' . unWindow
  where
    getWindowSize' win = uncurry V2 <$> GLFW.getWindowSize win

getGlfwWindowExtensions :: MonadIO io => Window GlfwWindowSystem -> io (Vector ByteString)
getGlfwWindowExtensions _ =
  liftIO $ GLFW.getRequiredInstanceExtensions >>= toByteStringVector
  where
    toByteStringVector = mapM packCString . Vector.fromList

pollGlfwWindowEvents :: MonadIO io => Window GlfwWindowSystem -> io [Event]
pollGlfwWindowEvents (GlfwWindow window) = liftIO $ do
  GLFW.pollEvents
  shouldClose <- GLFW.windowShouldClose window
  pure [QuitEvent | shouldClose]
