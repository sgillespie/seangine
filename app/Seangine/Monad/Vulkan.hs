module Seangine.Monad.Vulkan
  ( MonadVulkan(..),
    VulkanHandles(..),
    Vulkan(..),
    askVulkan,
    runVulkan
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Data.Word (Word32)

import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Control.Monad.Trans.Resource
import Vulkan.Core10
import VulkanMemoryAllocator

-- |Vulkan monad class. Contains convenience accessors to Vulkan handles
class Monad m => MonadVulkan m where
  getInstance :: m Instance
  getPhysicalDevice :: m PhysicalDevice
  getDevice :: m Device
  getAllocator :: m Allocator
  getGraphicsQueue :: m Queue
  getGraphicsQueueFamily :: m Word32
  getPresentQueue :: m Queue
  getPresentQueueFamily :: m Word32
  getCommandPool :: m CommandPool

instance MonadVulkan m => MonadVulkan (ReaderT r m) where
  getInstance = lift getInstance
  getPhysicalDevice = lift getPhysicalDevice
  getDevice = lift getDevice
  getAllocator = lift getAllocator
  getGraphicsQueue = lift getGraphicsQueue
  getGraphicsQueueFamily = lift getGraphicsQueueFamily
  getPresentQueue = lift getPresentQueue
  getPresentQueueFamily = lift getPresentQueueFamily
  getCommandPool = lift getCommandPool

-- |References to all relevant Vulkan handles
data VulkanHandles = VulkanHandles
  { vhInstance :: Instance,
    vhPhysicalDevice :: PhysicalDevice,
    vhDevice :: Device,
    vhAllocator :: Allocator,
    vhGraphicsQueue :: Queue,
    vhGraphicsQueueFamily :: Word32,
    vhPresentQueue :: Queue,
    vhPresentQueueFamily :: Word32,
    vhCommandPool :: CommandPool
  }

-- |Main Vulkan monad. Contains references to all relevant Vulkan handles
newtype Vulkan a
  = Vulkan { unVulkan :: ReaderT VulkanHandles (ResourceT IO) a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadFail,
            MonadIO,
            MonadResource
           )

instance MonadVulkan Vulkan where
  getInstance = Vulkan (asks vhInstance)
  getPhysicalDevice = Vulkan (asks vhPhysicalDevice)
  getDevice = Vulkan (asks vhDevice)
  getAllocator = Vulkan (asks vhAllocator)
  getGraphicsQueue = Vulkan (asks vhGraphicsQueue)
  getPresentQueue = Vulkan (asks vhPresentQueue)
  getGraphicsQueueFamily = Vulkan (asks vhGraphicsQueueFamily)
  getPresentQueueFamily = Vulkan (asks vhPresentQueueFamily)
  getCommandPool = Vulkan (asks vhCommandPool)

instance MonadUnliftIO Vulkan where
  withRunInIO a = Vulkan $ withRunInIO (\r -> a (r . unVulkan))

-- |Get the handles as a single value
askVulkan :: Vulkan VulkanHandles
askVulkan = Vulkan ask

-- |Runs a the given Vulkan monad using the provided handles. The resulting ResourceT
-- environment correctly destroys the Vulkan instance and all handles
runVulkan :: VulkanHandles -> Vulkan a -> ResourceT IO a
runVulkan handles = flip runReaderT handles . unVulkan
