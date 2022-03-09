module Graphics.Seangine.Init (withVulkanInstance, withInstance') where

import Graphics.Seangine.Monad

import Control.Monad.IO.Unlift (MonadIO(..))
import Control.Monad.Trans.Resource (MonadResource(..), allocate)
import Vulkan.CStruct.Extends (SomeStruct(SomeStruct))
import Vulkan.Core10
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Extensions.VK_KHR_shader_non_semantic_info
import Vulkan.Utils.Debug (debugCallbackPtr)
import Vulkan.Version
import Vulkan.Zero (Zero(..))
import VulkanMemoryAllocator (Allocator(..), AllocatorCreateInfo(..), withAllocator)
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Control.Monad (when)
import Control.Exception
import Data.Bits ((.|.), Bits(..))
import Data.List (maximumBy, nub)
import Data.Ord (comparing)
import Data.Traversable (for)
import Data.Word (Word32(..))

-- Constants
enabledLayers = ["VK_LAYER_KHRONOS_validation"]
extraExtensions = ["VK_EXT_debug_utils"]
deviceExtensions
  = [ KHR_SWAPCHAIN_EXTENSION_NAME,
      KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME
    ]

debugMsgSeverities :: [DebugUtilsMessageSeverityFlagBitsEXT]
debugMsgSeverities
  = [ -- DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT,
      DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT,
      DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT,
      DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
    ]

debugMsgTypes :: [DebugUtilsMessageTypeFlagBitsEXT]
debugMsgTypes
  = [ DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT,
      DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT,
      DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
    ]

withVulkanInstance
  :: MonadResource m
  => FilePath
  -> Instance
  -> SurfaceKHR
  -> m VulkanHandles
withVulkanInstance dataDir instance' surface = do
  physicalDevice <- choosePhysicalDevice instance'
  (gfxFamilyIndex, presentFamilyIndex) <- getQueueFamilyIndices physicalDevice surface
  device <- withDevice' physicalDevice [gfxFamilyIndex, presentFamilyIndex]
  allocator <- withAllocator' instance' physicalDevice device
  gfxQueue <- getDeviceQueue device gfxFamilyIndex 0
  presentQueue <- getDeviceQueue device presentFamilyIndex 0
  commandPool <- withCommandPool' device gfxFamilyIndex
  
  return VulkanHandles
    { vhDataDir = dataDir,
      vhInstance = instance',
      vhPhysicalDevice = physicalDevice,
      vhDevice = device,
      vhAllocator = allocator,
      vhGraphicsQueue = gfxQueue,
      vhGraphicsQueueFamily = gfxFamilyIndex,
      vhPresentQueue = presentQueue,
      vhPresentQueueFamily = presentFamilyIndex,
      vhCommandPool = undefined
    }

withInstance'
  :: MonadResource m
  => V.Vector B.ByteString -- ^ required window extensions
  -> m Instance
withInstance' exts = do
  let instanceCreateInfo = zero
        { applicationInfo = Just applicationInfo,
          enabledLayerNames = enabledLayers,
          enabledExtensionNames = extraExtensions <> exts
        }

      applicationInfo = zero
        { applicationName = Just "Seangine",
          applicationVersion = MAKE_API_VERSION 0 1 1,
          engineName = Just "Seangine"
        }

      debugMessengerInfo = zero
        { messageSeverity = foldr (.|.) zeroBits debugMsgSeverities,
          messageType = foldr (.|.) zeroBits debugMsgTypes,
          pfnUserCallback = debugCallbackPtr
        }
  
  (_, instance') <- withInstance instanceCreateInfo Nothing allocate
  _ <- withDebugUtilsMessengerEXT instance' debugMessengerInfo Nothing allocate
  
  return instance'

choosePhysicalDevice
  :: MonadIO m
  => Instance
  -> m PhysicalDevice
choosePhysicalDevice instance' = do
  (_, devices) <- enumeratePhysicalDevices instance'

  -- Score devices
  let score :: PhysicalDevice -> IO Int
      score device = do
        PhysicalDeviceProperties{..} <- getPhysicalDeviceProperties device
        return $ case deviceType of
          PHYSICAL_DEVICE_TYPE_DISCRETE_GPU -> 10
          PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU -> 5
          PHYSICAL_DEVICE_TYPE_CPU -> 2
          PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU -> 1
          _ -> 0

  scores <- liftIO $ mapM (\d -> score d >>= \s -> return (d, s)) devices

  let (res, _) = maximumBy (comparing snd) scores
  return res

getQueueFamilyIndices
  :: MonadIO m
  => PhysicalDevice
  -> SurfaceKHR
  -> m (Word32, Word32)
getQueueFamilyIndices device surface = do
  families <- getPhysicalDeviceQueueFamilyProperties device

  let indexed = V.indexed families
      isGraphicsFamily fam = queueFlags fam .&. QUEUE_GRAPHICS_BIT /= zeroBits
      isPresentFamily idx = getPhysicalDeviceSurfaceSupportKHR device idx surface

      graphicsFamIds = fromIntegral . fst <$> V.filter (isGraphicsFamily . snd) indexed

  presentFamIds <- V.map (fromIntegral . fst)
    <$> V.filterM (isPresentFamily . fromIntegral . fst) indexed

  when (V.any V.null [graphicsFamIds, presentFamIds]) $
    throwSystemError "Unable to find a graphics queue family"

  return (V.head graphicsFamIds, V.head graphicsFamIds)
  
withDevice'
  :: MonadResource m
  => PhysicalDevice
  -> [Word32]
  -> m Device
withDevice' physicalDevice queueIds = do
  let deviceInfo = zero
        { queueCreateInfos = V.fromList queueCreateInfos,
          enabledLayerNames = enabledLayers,
          enabledExtensionNames = deviceExtensions,
          enabledFeatures = Just features
        }

      queueCreateInfos = flip map (nub queueIds) $ \id ->
        SomeStruct $ zero { queueFamilyIndex = id, queuePriorities = [1] }

      features = zero
  
  snd <$> withDevice physicalDevice deviceInfo Nothing allocate

withAllocator'
  :: MonadResource m
  => Instance
  -> PhysicalDevice
  -> Device
  -> m Allocator
withAllocator' instance' physicalDevice device
  = snd <$> withAllocator allocInfo allocate
  where allocInfo = zero
          { physicalDevice = physicalDeviceHandle physicalDevice,
            device = deviceHandle device,
            instance' = instanceHandle instance'
          }

withCommandPool' :: MonadResource m => Device -> Word32 -> m CommandPool
withCommandPool' device queueFamilyIndex
  = snd <$> withCommandPool device createInfo Nothing allocate
  where createInfo = CommandPoolCreateInfo zero queueFamilyIndex
