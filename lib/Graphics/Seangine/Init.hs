module Graphics.Seangine.Init where

import Control.Monad.Trans.Resource (MonadResource(..), allocate)
import Vulkan.Core10
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Utils.Debug (debugCallbackPtr)
import Vulkan.Version
import Vulkan.Zero (Zero(..))
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Data.Bits ((.|.), Bits(..))

-- Constants
enabledLayers = ["VK_LAYER_KHRONOS_validation"]
extraExtensions = ["VK_EXT_debug_utils"]

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
