module Graphics.Seangine.Internal.SwapchainDetails
  (SwapchainDetails(..),
   withSwapchainDetails,
   withImageView'
  ) where

import Graphics.Seangine.Monad
import Graphics.Seangine.Window

import Control.Monad.Trans.Resource (allocate)
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Extensions.VK_KHR_surface (SurfaceCapabilitiesKHR(..), SurfaceFormatKHR(..))
import Vulkan.Zero (zero)

import Control.Arrow ((>>>))
import Data.Bits ((.&.))
import Data.Maybe
import Data.Word (Word32())
import qualified Data.Vector as V

-- constants
preferredSurfaceFormat :: Format
preferredSurfaceFormat = FORMAT_B8G8R8A8_SRGB

preferredColorSpace :: ColorSpaceKHR
preferredColorSpace = COLOR_SPACE_SRGB_NONLINEAR_KHR

preferredPresentMode :: PresentModeKHR
preferredPresentMode = PRESENT_MODE_FIFO_RELAXED_KHR

preferredDepthFormats :: [Format]
preferredDepthFormats
  = [ FORMAT_D32_SFLOAT,
      FORMAT_D32_SFLOAT_S8_UINT,
      FORMAT_D24_UNORM_S8_UINT 
    ]

data SwapchainDetails = SwapchainDetails
  { sdSwapchain :: SwapchainKHR,
    sdExtent :: Extent2D,
    sdSurfaceFormat :: Format,
    sdDepthFormat :: Format
  }

withSwapchainDetails
  :: WindowSystem system
  => Window system window
  -> SurfaceKHR
  -> SeangineInstance SwapchainDetails
withSwapchainDetails window surface = do
  device <- getDevice
  surfaceFormats <- getSurfaceFormats
  presentModes <- getPresentModes
  graphicsFamily <- getGraphicsQueueFamily
  presentFamily <- getPresentQueueFamily
  surfaceCapabilities@SurfaceCapabilitiesKHR{..} <- getSurfaceCapabilities

  let SurfaceFormatKHR{..} = chooseSurfaceFormat surfaceFormats
      presentMode = choosePresentMode presentModes
      sharingMode = chooseImageSharingMode graphicsFamily presentFamily
      queueFamilies = chooseQueueFamilyIndices sharingMode graphicsFamily presentFamily

  swapExtent <- chooseSwapExtent window
  depthFormat <- chooseDepthFormat

  let createInfo = zero
        { surface = surface,
          minImageCount = minImageCount + 1,
          imageFormat = format,
          imageColorSpace = colorSpace,
          imageExtent = swapExtent,
          imageArrayLayers = 1,
          imageUsage = IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
          imageSharingMode = sharingMode,
          queueFamilyIndices = queueFamilies,
          preTransform = currentTransform,
          compositeAlpha = COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
          presentMode = presentMode,
          clipped = True
        }

  (_, swapchain) <- withSwapchainKHR device createInfo Nothing allocate

  return SwapchainDetails
    { sdSwapchain = swapchain,
      sdExtent = swapExtent,
      sdSurfaceFormat = format,
      sdDepthFormat = depthFormat
    }

withImageView' :: Image -> Format -> ImageAspectFlags -> SeangineInstance ImageView
withImageView' image format flags = do
  device <- getDevice

  let createInfo = zero
        { image = image,
          viewType = IMAGE_VIEW_TYPE_2D,
          format = format,
          components = zero,
          subresourceRange = subresourceRange
        }

      subresourceRange = zero
        { aspectMask = flags,
          baseMipLevel = 0,
          levelCount = 1,
          baseArrayLayer = 0,
          layerCount = 1
        }

  snd <$> withImageView device createInfo Nothing allocate

chooseSurfaceFormat :: V.Vector SurfaceFormatKHR -> SurfaceFormatKHR
chooseSurfaceFormat surfaceFormats
  = fromMaybe
      (V.head surfaceFormats)
      (V.find isPreferredFormat surfaceFormats)
  where isPreferredFormat SurfaceFormatKHR{..}
          = format == preferredSurfaceFormat
          && colorSpace == preferredColorSpace

choosePresentMode :: V.Vector PresentModeKHR -> PresentModeKHR
choosePresentMode presentModes
  = fromMaybe
      PRESENT_MODE_FIFO_KHR
      (V.find (preferredPresentMode==) presentModes)

chooseSwapExtent :: WindowSystem system => Window system window -> SeangineInstance Extent2D
chooseSwapExtent window = do
  SurfaceCapabilitiesKHR{currentExtent=currentExtent} <- getSurfaceCapabilities
  let (Extent2D width height) = currentExtent

  if width == maxBound && height == maxBound
      then windowExtent window
      else return currentExtent

chooseDepthFormat :: SeangineInstance Format
chooseDepthFormat = do
  device <- getPhysicalDevice

  allFormatProperties <-
    mapM (getPhysicalDeviceFormatProperties device) preferredDepthFormats

  let requiredFeature = FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT

      isSuitableFormat (format, FormatProperties{..})
        = optimalTilingFeatures .&. requiredFeature == requiredFeature

      maybeDepthFormat
        = ($ allFormatProperties)
        $ zip preferredDepthFormats
        >>> filter isSuitableFormat
        >>> map fst
        >>> listToMaybe

  case maybeDepthFormat of
    Nothing -> throwSystemError "Failed to find a supported format"
    Just format -> return format

chooseImageSharingMode :: Word32 -> Word32 -> SharingMode
chooseImageSharingMode graphicsQueue presentQueue
  | graphicsQueue == presentQueue = SHARING_MODE_EXCLUSIVE
  | otherwise = SHARING_MODE_CONCURRENT

chooseQueueFamilyIndices :: SharingMode -> Word32 -> Word32 -> V.Vector Word32
chooseQueueFamilyIndices SHARING_MODE_CONCURRENT graphicsFamily presentFamily
  = [graphicsFamily, presentFamily]
chooseQueueFamilyIndices SHARING_MODE_EXCLUSIVE _ _ = []

windowExtent :: WindowSystem system => Window system window -> SeangineInstance Extent2D
windowExtent window = do
  (width, height) <- getDrawableSize window
  return $ Extent2D (fromIntegral width) (fromIntegral height)

