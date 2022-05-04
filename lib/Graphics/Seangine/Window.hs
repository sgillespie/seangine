module Graphics.Seangine.Window
  ( Window(..),
    WindowEvent(..),
    WindowSystem(..),
    withWindowSystem,
    withWindow,
    withWindowSurface,
    shouldQuit
  ) where

import Control.Monad.Trans.Resource
import Data.Text (pack)
import Linear.V2
import SDL.Video.Vulkan (vkCreateSurface, vkGetDrawableSize, vkGetInstanceExtensions)
import Vulkan.Core10 (Instance(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR(..), destroySurfaceKHR)
import qualified SDL

import Control.Monad ((>=>), void)
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.C.String  (peekCString)
import Foreign.Ptr (castPtr)
import qualified Data.ByteString as B
import qualified Data.Vector as V

data WindowEvent
  = QuitEvent
  deriving (Eq, Show)

class WindowSystem system where
  data Window system :: * -> *

  initWindowSystem :: MonadIO io => system -> io ()
    
  destroyWindowSystem :: MonadIO io => system -> io ()
    
  createWindow
    :: MonadIO io
    => system
    -> String -- ^ Window title
    -> Int    -- ^ Window width
    -> Int    -- ^ Window height
    -> io (Window system window)

  destroyWindow :: MonadIO io => Window system window -> io ()

  getVulkanExtensions
    :: MonadIO io
    => Window system window
    -> io (V.Vector B.ByteString)
    
  createVulkanSurface
    :: MonadIO io
    => Instance
    -> Window system window
    -> io SurfaceKHR

  getDrawableSize
    :: MonadIO io
    => Window system window
    -> io (Int, Int)

  awaitWindowEvents
    :: MonadResource resource
    => Window system window
    -> resource [WindowEvent]

withWindowSystem
  :: (MonadResource m, WindowSystem system)
  => system
  -> m ReleaseKey
withWindowSystem sys = allocate_ (initWindowSystem sys) (destroyWindowSystem sys)

withWindow
  :: (MonadResource m, WindowSystem system)
  => system
  -> String -- ^ Window title
  -> Int    -- ^ Window width
  -> Int    -- ^ Window height
  -> m (ReleaseKey, Window system window)
withWindow system title width height
  = withWindowSystem system >> allocate createWindow' destroyWindow
  where createWindow' = createWindow system title width height

withWindowSurface
  :: (MonadResource m, WindowSystem system)
  => Instance
  -> Window system window
  -> m (ReleaseKey, SurfaceKHR)
withWindowSurface instance' window
  = allocate create destroy
  where create = createVulkanSurface instance' window
        destroy surface = destroySurfaceKHR instance' surface Nothing

shouldQuit :: [WindowEvent] -> Bool
shouldQuit = any shouldQuit'
  where shouldQuit' QuitEvent = True
