module Graphics.Seangine.Frame.SwapchainDetails
  ( SwapchainDetails (..),
    withSwapchainDetails,
  ) where

import Graphics.Seangine.Config.VulkanHandles
import Graphics.Seangine.Errors
import Graphics.Seangine.Monad
import Graphics.Seangine.Window

import Control.Monad.Trans.Resource
import Data.Bits
import Data.Vector (Vector)
import UnliftIO.Exception
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Zero

import qualified Data.Vector as Vector

-- constants
preferredSurfaceFormat :: Format
preferredSurfaceFormat = FORMAT_B8G8R8A8_SRGB

preferredColorSpace :: ColorSpaceKHR
preferredColorSpace = COLOR_SPACE_SRGB_NONLINEAR_KHR

preferredPresentMode :: PresentModeKHR
preferredPresentMode = PRESENT_MODE_FIFO_KHR

preferredDepthFormats :: [Format]
preferredDepthFormats =
  [ FORMAT_D32_SFLOAT,
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
  -> Vulkan SwapchainDetails
withSwapchainDetails window surface = do
  device <- getDevice
  surfaceFormats <- getSurfaceFormats
  presentModes <- getPresentModes
  graphicsFamily <- getGraphicsQueueFamily
  presentFamily <- getPresentQueueFamily
  SurfaceCapabilitiesKHR {..} <- getSurfaceCapabilities

  let presentMode = choosePresentMode presentModes
      sharingMode = chooseImageSharingMode graphicsFamily presentFamily
      queueFamilies = chooseQueueFamilyIndices sharingMode graphicsFamily presentFamily

  SurfaceFormatKHR {..} <- chooseSurfaceFormat surfaceFormats
  swapExtent <- chooseSwapExtent window
  depthFormat <- chooseDepthFormat

  let createInfo =
        zero
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

  pure
    SwapchainDetails
      { sdSwapchain = swapchain,
        sdExtent = swapExtent,
        sdSurfaceFormat = format,
        sdDepthFormat = depthFormat
      }

chooseSurfaceFormat :: Vector SurfaceFormatKHR -> Vulkan SurfaceFormatKHR
chooseSurfaceFormat formats = maybe (throwIO NoSurfaceFormatError) pure maybeFormat
  where
    maybeFormat =
      Vector.find isPreferredFormat formats <|> Vector.headM formats
    isPreferredFormat SurfaceFormatKHR {..} =
      format == preferredSurfaceFormat
        && colorSpace == preferredColorSpace

choosePresentMode :: Vector PresentModeKHR -> PresentModeKHR
choosePresentMode = fromMaybe PRESENT_MODE_FIFO_KHR . Vector.find (preferredPresentMode ==)

chooseImageSharingMode :: Word32 -> Word32 -> SharingMode
chooseImageSharingMode graphicsQueue presentQueue
  | graphicsQueue == presentQueue = SHARING_MODE_EXCLUSIVE
  | otherwise = SHARING_MODE_CONCURRENT

chooseQueueFamilyIndices :: SharingMode -> Word32 -> Word32 -> Vector Word32
chooseQueueFamilyIndices sharingMode graphicsQueue presentQueue =
  case sharingMode of
    SHARING_MODE_CONCURRENT -> [graphicsQueue, presentQueue]
    _ -> []

chooseSwapExtent
  :: WindowSystem system
  => Window system window
  -> Vulkan Extent2D
chooseSwapExtent window = do
  SurfaceCapabilitiesKHR {currentExtent = currentExtent} <- getSurfaceCapabilities
  let (Extent2D width height) = currentExtent

  if width == maxBound && height == maxBound
    then windowExtent window
    else pure currentExtent

windowExtent :: WindowSystem system => Window system window -> Vulkan Extent2D
windowExtent window = do
  (width, height) <- getDrawableSize window
  pure $ Extent2D (fromIntegral width) (fromIntegral height)

chooseDepthFormat :: Vulkan Format
chooseDepthFormat = do
  device <- getPhysicalDevice
  formatProperties <- mapM (getPhysicalDeviceFormatProperties device) preferredDepthFormats

  let requiredFeature = FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT
      isSuitableFormat (_, FormatProperties {..}) =
        optimalTilingFeatures .&. requiredFeature == requiredFeature
      maybeDepthFormat =
        ($ formatProperties) $
          zip preferredDepthFormats
            >>> filter isSuitableFormat
            >>> map fst
            >>> listToMaybe

  maybe (throwIO NoDepthFormatError) pure maybeDepthFormat
