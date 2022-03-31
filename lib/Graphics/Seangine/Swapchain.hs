module Graphics.Seangine.Swapchain (withSwapchain) where

import Graphics.Seangine.Monad
import Graphics.Seangine.Window (Window(), getDrawableSize)

import Control.Monad.Trans.Resource (allocate)
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Zero (Zero(..))

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Word (Word32())
import Data.Maybe (fromMaybe)
import GHC.Clock (getMonotonicTime)
import qualified Data.Vector as V

-- constants
preferredSurfaceFormat = FORMAT_B8G8R8A8_SRGB
preferredColorSpace = COLOR_SPACE_SRGB_NONLINEAR_KHR
preferredPresentMode = PRESENT_MODE_FIFO_RELAXED_KHR

withSwapchain :: Window -> SurfaceKHR -> Vulkan SwapchainKHR
withSwapchain window surface = do
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

  return swapchain

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

chooseSwapExtent :: Window -> Vulkan Extent2D
chooseSwapExtent window = do
  SurfaceCapabilitiesKHR{currentExtent=currentExtent} <- getSurfaceCapabilities
  let (Extent2D width height) = currentExtent

  if width == maxBound && height == maxBound
      then windowExtent window
      else return currentExtent

chooseImageSharingMode :: Word32 -> Word32 -> SharingMode
chooseImageSharingMode graphicsQueue presentQueue
  | graphicsQueue == presentQueue = SHARING_MODE_EXCLUSIVE
  | otherwise = SHARING_MODE_CONCURRENT

chooseQueueFamilyIndices :: SharingMode -> Word32 -> Word32 -> V.Vector Word32
chooseQueueFamilyIndices SHARING_MODE_CONCURRENT graphicsFamily presentFamily
  = [graphicsFamily, presentFamily]
chooseQueueFamilyIndices SHARING_MODE_EXCLUSIVE _ _ = []

windowExtent :: Window -> Vulkan Extent2D
windowExtent window = uncurry Extent2D <$> getDrawableSize window
