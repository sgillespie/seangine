module Graphics.Seangine.Monad.Instance
  ( MonadInstance(..),
    InstanceHandles(..),
    SeangineInstance(..),
    askInstance,
    runInstance
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Data.Vector (Vector)
import Data.Word (Word32)

import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Control.Monad.Trans.Resource
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR, SurfaceCapabilitiesKHR, SurfaceFormatKHR)
import VulkanMemoryAllocator

-- |Monad class containing all Vulkan instance-level resources. Once these are created,
-- they DO NOT need not be recreated along with the swapchain.
class Monad m => MonadInstance m where
  getDataDir :: m FilePath
  getInstance :: m Instance
  getPhysicalDevice :: m PhysicalDevice
  getDevice :: m Device
  getAllocator :: m Allocator
  getGraphicsQueue :: m Queue
  getGraphicsQueueFamily :: m Word32
  getPresentQueue :: m Queue
  getPresentQueueFamily :: m Word32
  getSurfaceCapabilities :: m SurfaceCapabilitiesKHR
  getSurfaceFormats :: m (Vector SurfaceFormatKHR)
  getPresentModes :: m (Vector PresentModeKHR)
  getCommandPool :: m CommandPool

instance MonadInstance m => MonadInstance (ReaderT r m) where
  getDataDir = lift getDataDir
  getInstance = lift getInstance
  getPhysicalDevice = lift getPhysicalDevice
  getDevice = lift getDevice
  getAllocator = lift getAllocator
  getGraphicsQueue = lift getGraphicsQueue
  getGraphicsQueueFamily = lift getGraphicsQueueFamily
  getPresentQueue = lift getPresentQueue
  getPresentQueueFamily = lift getPresentQueueFamily
  getSurfaceCapabilities = lift getSurfaceCapabilities
  getSurfaceFormats = lift getSurfaceFormats
  getPresentModes = lift getPresentModes
  getCommandPool = lift getCommandPool

data InstanceHandles = InstanceHandles
  { vhDataDir :: FilePath,
    vhInstance :: Instance,
    vhPhysicalDevice :: PhysicalDevice,
    vhDevice :: Device,
    vhAllocator :: Allocator,
    vhGraphicsQueue :: Queue,
    vhGraphicsQueueFamily :: Word32,
    vhPresentQueue :: Queue,
    vhPresentQueueFamily :: Word32,
    vhSurfaceCapabilities :: SurfaceCapabilitiesKHR,
    vhSurfaceFormats :: Vector SurfaceFormatKHR,
    vhPresentModes :: Vector PresentModeKHR,
    vhCommandPool :: CommandPool
  }

newtype SeangineInstance a
  = SeangineInstance { unInstance :: ReaderT InstanceHandles (ResourceT IO) a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadFail,
            MonadIO,
            MonadResource
           )

instance MonadInstance SeangineInstance where
  getDataDir = SeangineInstance (asks vhDataDir)
  getInstance = SeangineInstance (asks vhInstance)
  getPhysicalDevice = SeangineInstance (asks vhPhysicalDevice)
  getDevice = SeangineInstance (asks vhDevice)
  getAllocator = SeangineInstance (asks vhAllocator)
  getGraphicsQueue = SeangineInstance (asks vhGraphicsQueue)
  getPresentQueue = SeangineInstance (asks vhPresentQueue)
  getGraphicsQueueFamily = SeangineInstance (asks vhGraphicsQueueFamily)
  getPresentQueueFamily = SeangineInstance (asks vhPresentQueueFamily)
  getSurfaceCapabilities = SeangineInstance (asks vhSurfaceCapabilities)
  getSurfaceFormats = SeangineInstance (asks vhSurfaceFormats)
  getPresentModes = SeangineInstance (asks vhPresentModes)
  getCommandPool = SeangineInstance (asks vhCommandPool)

instance MonadUnliftIO SeangineInstance where
  withRunInIO a = SeangineInstance $ withRunInIO (\r -> a (r . unInstance))

-- |Get the handles as a single value
askInstance :: SeangineInstance InstanceHandles
askInstance = SeangineInstance ask

-- |Runs a the given Instance monad using the provided handles. The resulting ResourceT
-- environment correctly destroys all handles
runInstance :: InstanceHandles -> SeangineInstance a -> ResourceT IO a
runInstance handles = flip runReaderT handles . unInstance
