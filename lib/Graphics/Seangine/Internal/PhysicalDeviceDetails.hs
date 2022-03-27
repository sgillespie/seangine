module Graphics.Seangine.Internal.PhysicalDeviceDetails
  ( PhysicalDeviceDetails(..),
    physicalDeviceDetails,
    isDeviceSuitable,
    score
  ) where

import Control.Monad.IO.Unlift (MonadIO(..))
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface

import Data.Word (Word32(..))
import qualified Data.ByteString as B
import qualified Data.Vector as V

data PhysicalDeviceDetails = PhysicalDeviceDetails
  { ppdPhysicalDevice :: PhysicalDevice,
    ppdGraphicsFamilyIndex :: Word32,
    ppdPresentFamilyIndex :: Word32,
    ppdSurfaceCapabilities :: SurfaceCapabilitiesKHR,
    ppdSurfaceFormats :: V.Vector SurfaceFormatKHR,
    ppdPresentModes :: V.Vector PresentModeKHR
  }

physicalDeviceDetails
  :: PhysicalDevice
  -> Maybe Word32
  -> Maybe Word32
  -> SurfaceCapabilitiesKHR
  -> V.Vector SurfaceFormatKHR
  -> V.Vector PresentModeKHR
  -> Maybe PhysicalDeviceDetails
physicalDeviceDetails
  device
  graphicsFamilyQueue
  presentFamilyQueue
  surfaceCapabilities
  surfaceFormats
  presentModes
  =
  do
    graphicsFamilyQueue' <- graphicsFamilyQueue
    presentFamilyQueue' <- presentFamilyQueue
    surfaceFormats' <- nonemptyList surfaceFormats
    presentModes' <- nonemptyList presentModes
    
    Just $ PhysicalDeviceDetails
      { ppdPhysicalDevice = device,
        ppdGraphicsFamilyIndex = graphicsFamilyQueue',
        ppdPresentFamilyIndex = presentFamilyQueue',
        ppdSurfaceCapabilities = surfaceCapabilities,
        ppdSurfaceFormats = surfaceFormats',
        ppdPresentModes = presentModes'
      }

isDeviceSuitable :: MonadIO m => PhysicalDeviceDetails -> V.Vector B.ByteString -> m Bool
isDeviceSuitable PhysicalDeviceDetails{..} requiredExts = do
  (_, availableExts) <- enumerateDeviceExtensionProperties ppdPhysicalDevice Nothing
  let availableExtNames = V.map extensionName availableExts
      hasRequiredExts = all (`V.elem` availableExtNames) requiredExts

  return hasRequiredExts

score :: MonadIO m => PhysicalDevice -> m Int
score device = do
  PhysicalDeviceProperties{..} <- getPhysicalDeviceProperties device
  return $ case deviceType of
    PHYSICAL_DEVICE_TYPE_DISCRETE_GPU -> 10
    PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU -> 5
    PHYSICAL_DEVICE_TYPE_CPU -> 2
    PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU -> 1
    _ -> 0

sequenceMaybes :: (Maybe a, Maybe b) -> Maybe (a, b)
sequenceMaybes (Just a, Just b) = Just (a, b)
sequenceMaybes _ = Nothing                                   

nonemptyList :: Traversable t => t a -> Maybe (t a)
nonemptyList ls | null ls = Nothing
                | otherwise = Just ls
