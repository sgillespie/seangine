module Graphics.Seangine.Window
  (module SDL,
   withWindow,
   windowExts,
   withWindowSurface,
   awaitWindowEvents,
   shouldQuit
  ) where

import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import Data.Text (pack)
import qualified Data.Vector as V
import SDL hiding (Vector)
import SDL.Video.Vulkan (vkCreateSurface, vkGetInstanceExtensions)
import Vulkan.Core10 (Instance(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR(..), destroySurfaceKHR)

import Control.Monad ((>=>), void)
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.C.String  (peekCString)
import Foreign.Ptr (castPtr)

withWindow
  :: MonadResource m
  => String -- ^ Window title
  -> Int    -- ^ Window width
  -> Int    -- ^ Window height
  -> m (ReleaseKey, Window)
withWindow title width height = withSdl >> allocate create destroyWindow
  where create = do
          let config = defaultWindow
                { windowGraphicsContext = VulkanContext,
                  windowInitialSize = V2 (fromIntegral width) (fromIntegral height),
                  windowResizable = True,
                  windowMode = Windowed
                }

          createWindow (pack title) config

windowExts :: MonadIO m => Window -> m (V.Vector B.ByteString)
windowExts win = do
  exts <- vkGetInstanceExtensions win
  exts' <- liftIO $ mapM B.packCString exts

  return $ V.fromList exts'
          
withWindowSurface
  :: MonadResource m
  => Instance
  -> Window
  -> m (ReleaseKey, SurfaceKHR)
withWindowSurface instance' win = allocate create destroy
  where create = SurfaceKHR
               <$> vkCreateSurface win (castPtr (instanceHandle instance'))
        destroy = flip (destroySurfaceKHR instance') Nothing

awaitWindowEvents :: MonadResource m => m [SDL.Event]
awaitWindowEvents = (:) <$> waitEvent <*> pollEvents

shouldQuit :: [SDL.Event] -> Bool
shouldQuit = any shouldQuit'
  where shouldQuit' (Event _ QuitEvent) = True
        shouldQuit' _ = False

withSdl :: MonadResource m => m ()
withSdl = void $ allocate_ create quit
  where create = initialize ([InitEvents, InitVideo] :: [InitFlag])
