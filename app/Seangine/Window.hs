module Seangine.Window (withWindow, withWindowSurface) where

import Control.Exception
import Control.Monad (when)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable(..))

import Control.Monad.Trans.Resource
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface
import qualified Graphics.UI.GLFW as GLFW

withWindow :: (MonadResource m) => String -> Int -> Int -> m (ReleaseKey, GLFW.Window)
withWindow title width height = allocate create destroy
  where create = do
          _ <- GLFW.init
          GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)
          win <- GLFW.createWindow width height title Nothing Nothing
          case win of
            Just win' -> return win'
            _ -> throwIO $ userError "Failed to create window!"

        destroy win = do
          GLFW.destroyWindow win
          GLFW.terminate

withWindowSurface :: (MonadResource m) => Instance -> GLFW.Window -> m (ReleaseKey, SurfaceKHR)
withWindowSurface instance' window = allocate create destroy
  where create = alloca $ \ ptr -> do
          let instPtr = castPtr (instanceHandle instance')
              alloc' = nullPtr

          result <- GLFW.createWindowSurface instPtr window alloc' ptr :: IO Int
          when (result /= 0) $ do
            let res = Result $ fromIntegral result
                msg = "Failed to create window (" ++ show res ++ ")!"
            throwIO $ userError msg

          peek ptr

        destroy = flip (destroySurfaceKHR instance') Nothing

