module Graphics.Seangine.Frame
  ( withVulkanFrame,
    withCommandBuffers',

    module Graphics.Seangine.Frame.UniformBufferObject,
    module Graphics.Seangine.Frame.Vertex,
    module Graphics.Seangine.Frame.VertexShader,
    module Graphics.Seangine.Frame.FragShader
  ) where

import Graphics.Seangine.Frame.BufferDetails
import Graphics.Seangine.Frame.DescriptorSets (withDescriptorSets')
import Graphics.Seangine.Frame.FragShader
import Graphics.Seangine.Frame.GraphicsPipelineDetails
import Graphics.Seangine.Frame.SwapchainDetails
import Graphics.Seangine.Frame.UniformBufferObject
import Graphics.Seangine.Frame.Vertex
import Graphics.Seangine.Frame.VertexShader
import Graphics.Seangine.Internal.Utils (throwIfUnsuccessful)
import Graphics.Seangine.Monad
import Graphics.Seangine.Monad.Frame
import Graphics.Seangine.Window

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Resource
import Data.Bits
import Data.Maybe (fromJust)
import Data.Traversable (for)
import Data.Word (Word32())
import Foreign.Marshal.Array (pokeArray)
import Foreign.C.Types (CUShort())
import Foreign.Storable (Storable(..))
import Foreign.Ptr (castPtr)
import GHC.Clock (getMonotonicTime)
import Linear (V2(..), V3(..))
import Lens.Micro
import Text.GLTF.Loader
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR())
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Zero (Zero(..))
import qualified VulkanMemoryAllocator as VMA
import qualified Data.Vector as V

withVulkanFrame
  :: WindowSystem system
  => Window system window
  -> SurfaceKHR
  -> Scene
  -> SeangineInstance Frame
withVulkanFrame window surface scene = do
  handles <- SeangineInstance ask
  allocator <- getAllocator

  start <- liftIO getMonotonicTime

  let meshPrimitives = getMeshPrimitives scene
      indices = concatMap (^. _meshPrimitiveIndices) meshPrimitives
      positions = concatMap (^. _meshPrimitivePositions) meshPrimitives
      vertices = map (\pos -> Vertex pos (V3 1 0 0) (V2 0 0)) positions

  swapchainDetails@SwapchainDetails{..} <- withSwapchainDetails window surface
  imageViews <- withImageViews swapchainDetails
  GraphicsPipelineDetails{..} <- withGraphicsPipelineDetails swapchainDetails
  depthImageView <- withDepthImageView swapchainDetails
  framebuffers <- withFramebuffers imageViews depthImageView renderPass sdExtent
  (imageAvailable, renderFinished) <- withSemaphores
  vertexBuffer <- withVertexBuffer vertices
  indexBuffer <- withIndexBuffer indices
  uniformBuffers <- withUniformBuffer imageViews
  resources <- allocate createInternalState closeInternalState
  descriptorSets <-
    withDescriptorSets' imageViews (V.map fst uniformBuffers) descriptorSetLayout
  fence <- withFence'
  
  return Frame
    { fIndex = 0,
      fStartTime = start,
      fScene = scene,
      fSurface = surface,
      fSwapchain = sdSwapchain,
      fImageExtent = sdExtent,
      fRenderPass = renderPass,
      fFramebuffers = framebuffers,
      fPipelineLayout = pipelineLayout,
      fGraphicsPipeline = graphicsPipeline,
      fImageAvailable = imageAvailable,
      fRenderFinished = renderFinished,
      fVertexBuffer = vertexBuffer,
      fIndexBuffer = indexBuffer,
      fUniformBuffers = uniformBuffers,
      fDescriptorSets = (descriptorSets V.!) . fromIntegral, 
      fResources = resources,
      fGpuWork = fence
    }

withCommandBuffers' :: Frame -> SeangineInstance (V.Vector CommandBuffer)
withCommandBuffers' Frame{..} = do
  commandPool <- getCommandPool
  device <- getDevice

  let commandBufferAllocateInfo = zero
        { commandPool = commandPool,
          level = COMMAND_BUFFER_LEVEL_PRIMARY,
          commandBufferCount = fromIntegral $ V.length fFramebuffers
        }
  
  snd <$> withCommandBuffers device commandBufferAllocateInfo allocate

getMeshPrimitives :: Scene -> [MeshPrimitive]
getMeshPrimitives scene
  = let nodes = scene ^. _nodes
        meshIds = mapM (^. _nodeMeshId) nodes
        meshes = maybe [] (map ((scene ^. _meshes) !!)) meshIds
    in concatMap (^. _meshPrimitives) meshes

withImageViews :: SwapchainDetails -> SeangineInstance (V.Vector ImageView)
withImageViews SwapchainDetails{..} = do
  device <- getDevice

  (res, images) <- getSwapchainImagesKHR device sdSwapchain
  throwIfUnsuccessful "Unable to get swapchain images" res
  
  for images $ \image ->
    withImageView' image sdSurfaceFormat IMAGE_ASPECT_COLOR_BIT

withFramebuffers
  :: V.Vector ImageView
  -> ImageView
  -> RenderPass
  -> Extent2D
  -> SeangineInstance (V.Vector Framebuffer)
withFramebuffers imageViews depthImageView renderPass imageExtent = do
  device <- getDevice

  let framebufferCreateInfo :: ImageView -> FramebufferCreateInfo '[]
      framebufferCreateInfo imageView = zero
        { attachments = [imageView, depthImageView],
          renderPass = renderPass,
          width = imageWidth,
          height = imageHeight,
          layers = 1
        }

      (Extent2D imageWidth imageHeight) = imageExtent

  for imageViews $ \imageView ->
    snd <$> withFramebuffer device (framebufferCreateInfo imageView) Nothing allocate

withDepthImageView :: SwapchainDetails -> SeangineInstance ImageView
withDepthImageView swapchain@SwapchainDetails{..} = do
  depthImage <- withDepthImage swapchain
  withImageView' depthImage sdDepthFormat IMAGE_ASPECT_DEPTH_BIT

withSemaphores :: SeangineInstance (Semaphore, Semaphore)
withSemaphores = do
  device <- getDevice

  let withSemaphore' createInfo = snd <$> withSemaphore device createInfo Nothing allocate

  imageAvailable <- withSemaphore' zero
  renderFinished <- withSemaphore' zero

  return (imageAvailable, renderFinished)

withDepthImage :: SwapchainDetails -> SeangineInstance Image
withDepthImage SwapchainDetails{..} = do
  allocator <- getAllocator
  device <- getDevice
  
  let (Extent2D width height) = sdExtent
      imageCreateInfo = zero
        { format = sdDepthFormat,
          imageType = IMAGE_TYPE_2D,
          extent = Extent3D width height 1,
          mipLevels = 1,
          arrayLayers = 1,
          samples = SAMPLE_COUNT_1_BIT,
          tiling = IMAGE_TILING_OPTIMAL,
          usage = IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT,
          sharingMode = SHARING_MODE_EXCLUSIVE
        }

      allocateCreateInfo = zero
        { VMA.requiredFlags = MEMORY_PROPERTY_DEVICE_LOCAL_BIT }
  
  (_, createImageResult) <-
    VMA.withImage allocator imageCreateInfo allocateCreateInfo allocate
  let (depthImage, _, _) = createImageResult

  return depthImage

withVertexBuffer :: [Vertex] -> SeangineInstance Buffer
withVertexBuffer vertices = do
  let bufferSize = fromIntegral $ sizeOf (zero :: Vertex) * length vertices
      usageFlags = BUFFER_USAGE_VERTEX_BUFFER_BIT

  bdBuffer <$> withDeviceLocalBuffer bufferSize usageFlags vertices

withIndexBuffer :: [Int] -> SeangineInstance Buffer
withIndexBuffer vertexIndices = do
  let bufferSize = fromIntegral $ sizeOf (undefined :: CUShort) * length vertexIndices
      usageFlags = BUFFER_USAGE_INDEX_BUFFER_BIT
      vertexIndices' = map (\i -> fromIntegral i :: CUShort) vertexIndices
  
  bdBuffer <$> withDeviceLocalBuffer bufferSize usageFlags vertexIndices'

withUniformBuffer :: V.Vector ImageView -> SeangineInstance (V.Vector (Buffer, VMA.Allocation))
withUniformBuffer imageViews = do
  allocator <- getAllocator

  let bufferSize = fromIntegral $ sizeOf (zero :: UniformBufferObject)
      usageFlags = BUFFER_USAGE_UNIFORM_BUFFER_BIT
      memoryFlags = MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT

  forM imageViews $ \_ -> do
    BufferDetails{..} <- withBufferDetails bufferSize usageFlags memoryFlags
    return (bdBuffer, bdAllocation)

withFence' :: SeangineInstance Fence
withFence' = do
  device <- getDevice

  snd <$> withFence device zero Nothing allocate
