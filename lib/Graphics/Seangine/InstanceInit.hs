module Graphics.Seangine.InstanceInit
  ( withInstanceHandles,
    withVulkanInstance
  ) where

import Graphics.Seangine.Monad
import Graphics.Seangine.InstanceInit.CommandBuffers (withCommandPool', withCommandBuffer')
import Graphics.Seangine.InstanceInit.PhysicalDeviceDetails
import Graphics.Seangine.Shared.Utils (mresult)

import Control.Exception
import Control.Monad
import Control.Monad.IO.Unlift (MonadIO(..))
import Control.Monad.Trans.Resource (MonadResource(..), allocate)
import Data.Bits ((.|.), Bits(..))
import Data.Foldable.Extra (findM)
import Data.Functor ((<&>))
import Data.List (maximumBy, nub)
import Data.Maybe (fromJust, isJust)
import Data.Ord (comparing)
import Data.Traversable (for)
import Data.Word (Word32(..))
import Foreign (castFunPtr)
import RIO
import Vulkan.CStruct.Extends (SomeStruct(SomeStruct))
import Vulkan.Core10
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Dynamic (DeviceCmds(..), InstanceCmds(..))
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Extensions.VK_EXT_validation_features
import Vulkan.Extensions.VK_KHR_shader_non_semantic_info
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Utils.Debug (debugCallbackPtr)
import Vulkan.Version
import Vulkan.Zero (Zero(..))
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified VulkanMemoryAllocator as VMA

-- Constants
enabledLayers :: V.Vector B.ByteString
enabledLayers = ["VK_LAYER_KHRONOS_validation"]
extraExtensions = ["VK_EXT_debug_utils"]
deviceExtensions
  = [KHR_SWAPCHAIN_EXTENSION_NAME]

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

withInstanceHandles
  :: MonadResource m
  => FilePath
  -> Instance
  -> SurfaceKHR
  -> m InstanceHandles
withInstanceHandles dataDir instance' surface = do
  PhysicalDeviceDetails{..} <- choosePhysicalDevice instance' surface
  device <- withDevice' ppdPhysicalDevice [ppdGraphicsFamilyIndex, ppdPresentFamilyIndex]
  allocator <- withAllocator' instance' ppdPhysicalDevice device
  gfxQueue <- getDeviceQueue device ppdGraphicsFamilyIndex 0
  presentQueue <- getDeviceQueue device ppdPresentFamilyIndex 0
  commandPool <- withCommandPool' device ppdGraphicsFamilyIndex zero

  return InstanceHandles
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
  => V.Vector B.ByteString -- ^ required window extensions
  -> m Instance
withVulkanInstance exts = do
  let instanceCreateInfo = zero
        { next = (debugMessengerInfo, (validationFeatures, ())),
          applicationInfo = Just applicationInfo,
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

      
      validationFeatures = ValidationFeaturesEXT
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
    [] -> throwSystemError "Can't find a suitable device!"
    _  -> do
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
      Just (graphicsQueueFamily, presentQueueFamily) -> do
        surfaceCapabilities <- getPhysicalDeviceSurfaceCapabilitiesKHR device surface
        surfaceFormats <- mresult <$> getPhysicalDeviceSurfaceFormatsKHR device surface
        presentModes <- mresult <$> getPhysicalDeviceSurfacePresentModesKHR device surface

        physicalDeviceDetails device graphicsQueueFamily presentQueueFamily
          <$> getPhysicalDeviceSurfaceCapabilitiesKHR device surface
          <*> fmap mresult (getPhysicalDeviceSurfaceFormatsKHR device surface)
          <*> fmap mresult (getPhysicalDeviceSurfacePresentModesKHR device surface)

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

      graphicsFamId = fromIntegral . fst
        <$> V.find (isGraphicsFamily . snd) indexedFamilies

  presentFamId <- fmap (fromIntegral . fst)
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
  -> m VMA.Allocator
withAllocator' instance' physicalDevice device
  = snd <$> VMA.withAllocator allocInfo allocate
  where allocInfo = zero
          { VMA.physicalDevice = physicalDeviceHandle physicalDevice,
            VMA.device = deviceHandle device,
            VMA.instance' = instanceHandle instance',
            VMA.vulkanFunctions = Just $ vulkanFunctions' instance' device
          }

vulkanFunctions' :: Instance -> Device -> VMA.VulkanFunctions
vulkanFunctions' instance' device = zero
  { VMA.vkGetInstanceProcAddr = castFunPtr pVkGetInstanceProcAddr,
    VMA.vkGetDeviceProcAddr = castFunPtr pVkGetDeviceProcAddr
  }
  where Instance _ InstanceCmds{..} = instance'
        Device _ DeviceCmds{..} = device
