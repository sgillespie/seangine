module Graphics.Seangine.Config.Frame
  ( Frame (..),
    FrameInFlight (..),
    HasFrame (..),
    HasFrameInFlight (..),
  ) where

import Graphics.Seangine.Scene

import Control.Monad.Trans.Resource (InternalState (), ReleaseKey ())
import Data.Vector (Vector)
import Text.GLTF.Loader (Gltf (..))
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_swapchain (SurfaceKHR (), SwapchainKHR ())
import VulkanMemoryAllocator (Allocation ())

data Frame = Frame
  { fIndex :: Word64,
    fStartTime :: Double,
    fScene :: Gltf,
    fSurface :: SurfaceKHR,
    fSwapchain :: SwapchainKHR,
    fImageExtent :: Extent2D,
    fRenderPass :: RenderPass,
    fFramebuffers :: Vector Framebuffer,
    fPipelineLayout :: PipelineLayout,
    fGraphicsPipeline :: Pipeline,
    fVertexBuffers :: HashMap MeshPrimitiveId Buffer,
    fIndexBuffers :: HashMap MeshPrimitiveId Buffer,
    fMaxFramesInFlight :: Int,
    fFramesInFlight :: Vector FrameInFlight,
    fResources :: (ReleaseKey, InternalState)
  }

data FrameInFlight = FrameInFlight
  { ffImageAvailable :: Semaphore,
    ffRenderFinished :: Semaphore,
    ffUniformBuffer :: (Buffer, Allocation),
    ffObjectBuffer :: (Buffer, Allocation),
    ffDescriptorSets :: Vector DescriptorSet,
    ffCommandPool :: CommandPool,
    ffCommandBuffer :: CommandBuffer,
    ffGpuWork :: Fence
  }

class HasFrame m where
  getFrame :: m Frame

class HasFrame m => HasFrameInFlight m where
  getFrameInFlight :: m FrameInFlight
