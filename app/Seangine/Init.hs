module Seangine.Init (withInstance', withVulkanHandles) where

import Data.Bits
import Data.Ord (comparing)
import Data.Word
import Data.List
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Control.Monad.IO.Unlift
import Control.Monad.Trans.Resource
import Vulkan.CStruct.Extends
import Vulkan.Core10
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Extensions.VK_EXT_validation_features
import Vulkan.Extensions.VK_KHR_shader_non_semantic_info
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Utils.Debug (debugCallbackPtr)
import Vulkan.Version
import Vulkan.Zero
import VulkanMemoryAllocator hiding (getPhysicalDeviceProperties)

import Seangine.Monad

-- Constants
debugMsgSeverities :: [DebugUtilsMessageSeverityFlagBitsEXT]
debugMsgSeverities =
  [
    -- DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT,
    DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT,
    DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT,
    DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
  ]

debugMsgTypes :: [DebugUtilsMessageTypeFlagBitsEXT]
debugMsgTypes =
  [ DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT,
    DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT,
    DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
  ]

withVulkanHandles
  :: (MonadResource m, MonadThrow m)
  => Instance
  -> SurfaceKHR
  -> m VulkanHandles
withVulkanHandles instance' surface = do
  physicalDevice <- choosePhysicalDevice instance'
  (graphicsFamilyIndex, presentFamilyIndex)
    <- getQueueFamilyIndices physicalDevice surface
  device <- withDevice' physicalDevice [graphicsFamilyIndex, presentFamilyIndex]
  allocator <- withAllocator' instance' physicalDevice device
  graphicsQueue <- getDeviceQueue device graphicsFamilyIndex 0
  presentQueue <- getDeviceQueue device presentFamilyIndex 0
  commandPool <- withCommandPool' device graphicsFamilyIndex

  return VulkanHandles
    { vhInstance = instance',
      vhPhysicalDevice = physicalDevice,
      vhDevice = device,
      vhAllocator = allocator,
      vhGraphicsQueue = graphicsQueue,
      vhGraphicsQueueFamily = graphicsFamilyIndex,
      vhPresentQueue = presentQueue,
      vhPresentQueueFamily = presentFamilyIndex,
      vhCommandPool = commandPool
    }

withInstance' :: MonadResource m => V.Vector B.ByteString -> m Instance
withInstance' requiredExts = do
  let 
      applicationInfo' = ApplicationInfo
        { applicationName = Just "Hello, Triangle!",
          applicationVersion = MAKE_VERSION 0 1 0,
          engineName = Just "Seangine",
          engineVersion = MAKE_VERSION 0 1 0,
          apiVersion = API_VERSION_1_0
        }

      instanceCreateInfo = InstanceCreateInfo
        { next = (debugCreateInfo, (validationFeatures, ())),
          flags = zero,
          applicationInfo = Just applicationInfo',
          enabledLayerNames = ["VK_LAYER_KHRONOS_validation"],
          enabledExtensionNames = ["VK_EXT_debug_utils"] <> requiredExts
        }

      debugCreateInfo = DebugUtilsMessengerCreateInfoEXT
        { flags = zero,
          messageSeverity = foldr (.|.) zeroBits debugMsgSeverities,
          messageType = foldr (.|.) zeroBits debugMsgTypes,
          pfnUserCallback = debugCallbackPtr,
          userData = zero
        }

      validationFeatures = ValidationFeaturesEXT
        { enabledValidationFeatures = [VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT],
          disabledValidationFeatures = []
        }

  (_, instance') <- withInstance instanceCreateInfo Nothing allocate
  _ <- withDebugUtilsMessengerEXT instance' debugCreateInfo Nothing allocate

  return instance'

choosePhysicalDevice
  :: MonadIO m
  => Instance
  -> m PhysicalDevice
choosePhysicalDevice inst = do
  (_, devices) <- enumeratePhysicalDevices inst

  -- Score devices
  let score :: PhysicalDevice -> IO Int
      score dev = do
        PhysicalDeviceProperties{..} <- getPhysicalDeviceProperties dev
        return $ case deviceType of
          PHYSICAL_DEVICE_TYPE_DISCRETE_GPU -> 10
          PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU -> 5
          PHYSICAL_DEVICE_TYPE_CPU -> 2
          PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU -> 1
          _ -> 0

  scores <- liftIO $ mapM (\d -> score d >>= \s -> return (d, s)) devices

  let (res, _) = maximumBy (comparing snd) scores
  return res

getQueueFamilyIndices :: MonadIO m => PhysicalDevice -> SurfaceKHR -> m (Word32, Word32)
getQueueFamilyIndices physicalDevice surface = do
  families <- getPhysicalDeviceQueueFamilyProperties physicalDevice

  let indexed = V.indexed families

      isGraphicsFamily fam = queueFlags fam .&. QUEUE_GRAPHICS_BIT /= zeroBits
      isPresentationFamily device idx = getPhysicalDeviceSurfaceSupportKHR device idx surface

      graphicsFamilyIndex
        = V.head $ fromIntegral . fst
          <$> V.filter (isGraphicsFamily . snd) indexed

  presentFamilyIndex
      <- V.head <$> V.filterM
           (isPresentationFamily physicalDevice)
           (V.generate (V.length families) fromIntegral)
 
  return (graphicsFamilyIndex, presentFamilyIndex)


withDevice'
  :: MonadResource m
  => PhysicalDevice
  -> [Word32]
  -> m Device
withDevice' physicalDevice queueIndices = do
  let createInfos = fmap
        (SomeStruct . createQueueInfo)
        (nub queueIndices)

      deviceCreateInfo = DeviceCreateInfo
        { next = (),
          flags = zero,
          queueCreateInfos = V.fromList createInfos,
          enabledLayerNames = ["VK_LAYER_KHRONOS_validation"],
          enabledExtensionNames
            = [ KHR_SWAPCHAIN_EXTENSION_NAME,
                KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME
              ],
          enabledFeatures = Nothing
        }

      createQueueInfo idx = DeviceQueueCreateInfo () zero idx [1]

  (_, device) <- withDevice physicalDevice deviceCreateInfo Nothing allocate
  return device

withAllocator' :: MonadResource m => Instance -> PhysicalDevice -> Device -> m Allocator
withAllocator' instance' physicalDevice device
  = snd <$> withAllocator allocInfo allocate
  where allocInfo = AllocatorCreateInfo
          { flags = zero,
            physicalDevice = physicalDeviceHandle physicalDevice,
            device = deviceHandle device,
            preferredLargeHeapBlockSize = zero,
            allocationCallbacks = Nothing,
            deviceMemoryCallbacks = Nothing,
            frameInUseCount = zero,
            heapSizeLimit = zero,
            vulkanFunctions = Nothing,
            recordSettings = Nothing,
            instance' = instanceHandle instance',
            vulkanApiVersion = API_VERSION_1_0
          }

withCommandPool' :: MonadResource m => Device -> Word32 -> m CommandPool
withCommandPool' device queueFamilyIndex
  = snd <$> withCommandPool device commandPoolInfo Nothing allocate
  where commandPoolInfo = CommandPoolCreateInfo
          { flags = zero,
            queueFamilyIndex = queueFamilyIndex
          }
