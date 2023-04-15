module Graphics.Seangine.Instance
  ( withVulkanHandles,
    withVulkanInstance,
  ) where

import Graphics.Seangine.Errors (SeangineError (..))
import Graphics.Seangine.Instance.CommandBuffers (withCommandPool')
import Graphics.Seangine.Instance.PhysicalDeviceDetails
import Graphics.Seangine.Monad
import Graphics.Seangine.Shared.Utils (fromResult)

import Control.Monad.Trans.Resource
import Data.Bits (Bits (..), (.|.))
import qualified Data.ByteString as B
import Data.Foldable.Extra (findM)
import Data.List (maximumBy)
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Foreign (castFunPtr)
import UnliftIO.Exception (throwIO)
import Vulkan.CStruct.Extends (SomeStruct (SomeStruct))
import Vulkan.Core10
import Vulkan.Dynamic (DeviceCmds (..), InstanceCmds (..))
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Extensions.VK_EXT_validation_features
import Vulkan.Extensions.VK_KHR_shader_draw_parameters
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Utils.Debug (debugCallbackPtr)
import Vulkan.Version
import Vulkan.Zero (Zero (..))

import qualified VulkanMemoryAllocator as VMA

-- Constants
enabledLayers :: V.Vector B.ByteString
enabledLayers =
  [ "VK_LAYER_KHRONOS_validation"
  ]

extraExtensions :: V.Vector ByteString
extraExtensions = ["VK_EXT_debug_utils"]

deviceExtensions :: V.Vector ByteString
deviceExtensions =
  [ KHR_SWAPCHAIN_EXTENSION_NAME,
    KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME
  ]

debugMsgSeverities :: [DebugUtilsMessageSeverityFlagBitsEXT]
debugMsgSeverities =
  [ -- DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT,
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
  :: MonadResource m
  => FilePath
  -> Instance
  -> SurfaceKHR
  -> m VulkanHandles
withVulkanHandles dataDir instance' surface = do
  PhysicalDeviceDetails {..} <- choosePhysicalDevice instance' surface
  device <- withDevice' ppdPhysicalDevice [ppdGraphicsFamilyIndex, ppdPresentFamilyIndex]
  allocator <- withAllocator' instance' ppdPhysicalDevice device
  gfxQueue <- getDeviceQueue device ppdGraphicsFamilyIndex 0
  presentQueue <- getDeviceQueue device ppdPresentFamilyIndex 0
  commandPool <- withCommandPool' device ppdGraphicsFamilyIndex zero

  return
    VulkanHandles
      { vhDataDir = dataDir,
        vhInstance = instance',
        vhPhysicalDevice = ppdPhysicalDevice,
        vhDevice = device,
        vhAllocator = allocator,
        vhGraphicsQueue = gfxQueue,
        vhGraphicsQueueFamily = ppdGraphicsFamilyIndex,
        vhPresentQueue = presentQueue,
        vhPresentQueueFamily = ppdPresentFamilyIndex,
        vhSurfaceCapabilities = ppdSurfaceCapabilities,
        vhSurfaceFormats = ppdSurfaceFormats,
        vhPresentModes = ppdPresentModes,
        vhCommandPool = commandPool
      }

withVulkanInstance
  :: MonadResource m
  => V.Vector B.ByteString
  -- ^ required window extensions
  -> m Instance
withVulkanInstance exts = do
  let instanceCreateInfo =
        zero
          { next = (debugMessengerInfo, (validationFeatures, ())),
            applicationInfo = Just applicationInfo,
            enabledLayerNames = enabledLayers,
            enabledExtensionNames = extraExtensions <> exts
          }

      applicationInfo =
        zero
          { applicationName = Just "Seangine",
            applicationVersion = MAKE_API_VERSION 0 1 1,
            engineName = Just "Seangine"
          }

      debugMessengerInfo =
        zero
          { messageSeverity = foldr (.|.) zeroBits debugMsgSeverities,
            messageType = foldr (.|.) zeroBits debugMsgTypes,
            pfnUserCallback = debugCallbackPtr
          }

      validationFeatures =
        ValidationFeaturesEXT
          { enabledValidationFeatures = [VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT],
            disabledValidationFeatures = []
          }

  (_, instance') <- withInstance instanceCreateInfo Nothing allocate
  _ <- withDebugUtilsMessengerEXT instance' debugMessengerInfo Nothing allocate

  return instance'

choosePhysicalDevice
  :: MonadIO m
  => Instance
  -> SurfaceKHR
  -> m PhysicalDeviceDetails
choosePhysicalDevice instance' surface = do
  devices <- getPhysicalDevices instance' surface

  suitableDevices <- V.filterM (`isDeviceSuitable` deviceExtensions) devices
  scores <- associateByM (score . ppdPhysicalDevice) suitableDevices

  case scores of
    [] -> throwIO NoDeviceError
    _ -> do
      let (res, _) = maximumBy (comparing snd) scores
      return res

associateByM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t (a, b))
associateByM f = mapM $ \l -> sequence (l, f l)

getPhysicalDevices :: MonadIO m => Instance -> SurfaceKHR -> m (V.Vector PhysicalDeviceDetails)
getPhysicalDevices instance' surface = do
  (_, devices) <- enumeratePhysicalDevices instance'

  allDevices <- forM devices $ \device -> do
    queueFamilyIndices <- getQueueFamilyIndices device surface
    case queueFamilyIndices of
      Nothing -> return Nothing
      Just (graphicsQueueFamily, presentQueueFamily) ->
        physicalDeviceDetails device graphicsQueueFamily presentQueueFamily
          <$> getPhysicalDeviceSurfaceCapabilitiesKHR device surface
          <*> fmap fromResult (getPhysicalDeviceSurfaceFormatsKHR device surface)
          <*> fmap fromResult (getPhysicalDeviceSurfacePresentModesKHR device surface)

  return $
    (V.map fromJust . V.filter isJust) allDevices

getQueueFamilyIndices
  :: MonadIO m
  => PhysicalDevice
  -> SurfaceKHR
  -> m (Maybe (Word32, Word32))
getQueueFamilyIndices device surface = do
  families <- getPhysicalDeviceQueueFamilyProperties device

  let indexedFamilies = V.indexed families
      isGraphicsFamily fam = queueFlags fam .&. QUEUE_GRAPHICS_BIT /= zeroBits
      isPresentFamily idx = getPhysicalDeviceSurfaceSupportKHR device idx surface

      graphicsFamId =
        fromIntegral . fst
          <$> V.find (isGraphicsFamily . snd) indexedFamilies

  presentFamId <-
    fmap (fromIntegral . fst)
      <$> findM (isPresentFamily . fromIntegral . fst) indexedFamilies

  case (presentFamId, graphicsFamId) of
    (Just presentFamId', Just graphicsFamId') -> return $ Just (presentFamId', graphicsFamId')
    _ -> return Nothing

withDevice'
  :: MonadResource m
  => PhysicalDevice
  -> [Word32]
  -> m Device
withDevice' physicalDevice queueIds = do
  let deviceInfo =
        zero
          { queueCreateInfos = V.fromList queueCreateInfos,
            enabledLayerNames = enabledLayers,
            enabledExtensionNames = deviceExtensions,
            enabledFeatures = Just features
          }

      queueCreateInfos = flip map (ordNub queueIds) $ \id' ->
        SomeStruct $ zero {queueFamilyIndex = id', queuePriorities = [1]}

      features = zero

  snd <$> withDevice physicalDevice deviceInfo Nothing allocate

withAllocator'
  :: MonadResource m
  => Instance
  -> PhysicalDevice
  -> Device
  -> m VMA.Allocator
withAllocator' instance' physicalDevice device =
  snd <$> VMA.withAllocator allocInfo allocate
  where
    allocInfo =
      zero
        { VMA.physicalDevice = physicalDeviceHandle physicalDevice,
          VMA.device = deviceHandle device,
          VMA.instance' = instanceHandle instance',
          VMA.vulkanFunctions = Just $ vulkanFunctions' instance' device
        }

vulkanFunctions' :: Instance -> Device -> VMA.VulkanFunctions
vulkanFunctions' instance' device =
  zero
    { VMA.vkGetInstanceProcAddr = castFunPtr pVkGetInstanceProcAddr,
      VMA.vkGetDeviceProcAddr = castFunPtr pVkGetDeviceProcAddr
    }
  where
    Instance _ InstanceCmds {..} = instance'
    Device _ DeviceCmds {..} = device
