module Graphics.Seangine.FrameInit
   ( withVulkanFrame,
     withCommandBuffers'
   ) where

import Graphics.Seangine.GraphicsPipeline
import Graphics.Seangine.Monad
import Graphics.Seangine.Render.UniformBufferObject
import Graphics.Seangine.Render.Vertex
import Graphics.Seangine.Scene
import Graphics.Seangine.Shared.Utils (throwIfUnsuccessful)
import Graphics.Seangine.FrameInit.BufferDetails
import Graphics.Seangine.FrameInit.DescriptorSets (withDescriptorSets')
import Graphics.Seangine.FrameInit.FrameInFlightInit (withFramesInFlight)
import Graphics.Seangine.FrameInit.SwapchainDetails
import Graphics.Seangine.Window

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Resource
import Data.Bits
import Data.Maybe (fromJust)
import Data.Traversable (for)
import Data.Word (Word32())
import Foreign.Marshal.Array (pokeArray)
import Foreign.C.Types
import Foreign.Storable (Storable(..))
import Foreign.Ptr (castPtr)
import GHC.Clock (getMonotonicTime)
import Linear (V2(..), V3(..))
import Lens.Micro
import Lens.Micro.Platform ()
import Prelude
import Text.GLTF.Loader
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR())
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Zero (Zero(..))
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as V
import qualified VulkanMemoryAllocator as VMA

maxFramesInFlight :: Int
maxFramesInFlight = 1

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
  swapchainDetails@SwapchainDetails{..} <- withSwapchainDetails window surface
  imageViews <- withImageViews swapchainDetails
  descriptorSetLayout <- withDescriptorSetLayout'
  pipelineLayout <- withPipelineLayout' descriptorSetLayout
  renderPass <- withRenderPass' sdSurfaceFormat sdDepthFormat
  graphicsPipeline <- withGraphicsPipeline' pipelineLayout renderPass sdExtent
  
  depthImageView <- withDepthImageView swapchainDetails
  framebuffers <- withFramebuffers imageViews depthImageView renderPass sdExtent
  vertexBuffers <- withVertexBuffers scene
  indexBuffers <- withIndexBuffers scene
  framesInFlight <- withFramesInFlight maxFramesInFlight descriptorSetLayout
  resources <- allocate createInternalState closeInternalState
  
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
      fVertexBuffers = vertexBuffers,
      fIndexBuffers = indexBuffers,
      fMaxFramesInFlight = maxFramesInFlight,
      fFramesInFlight = framesInFlight,
      fResources = resources
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

withVertexBuffers :: Scene -> SeangineInstance (Map.HashMap MeshPrimitiveId Buffer)
withVertexBuffers scene = V.foldM addNodeMeshToMap' Map.empty nodes
  where addNodeMeshToMap' = addNodeMeshToMap createBuffer scene
        nodes = scene ^. _nodes
        createBuffer primitive' = do
          let bufferSize = fromIntegral $ sizeOf (zero :: Vertex) * length vertices
              usageFlags = BUFFER_USAGE_VERTEX_BUFFER_BIT

              vertices = V.zipWith
                (\pos norm -> Vertex pos norm (abs norm))
                (primitive' ^. _meshPrimitivePositions)
                (primitive' ^. _meshPrimitiveNormals)
                  
          bdBuffer <$> withDeviceLocalBuffer bufferSize usageFlags vertices
          
    
withIndexBuffers :: Scene -> SeangineInstance (Map.HashMap MeshPrimitiveId Buffer)
withIndexBuffers scene = V.foldM addNodeMeshToMap' Map.empty nodes
  where nodes = scene ^. _nodes
        addNodeMeshToMap' = addNodeMeshToMap createBuffer scene
        fromIntegral' = CShort . fromIntegral
        createBuffer primitive' = do
          let indices = primitive' ^. _meshPrimitiveIndices & each %~ fromIntegral'
              bufferSize = fromIntegral $ sizeOf (undefined :: CUShort) * length indices
              usageFlags = BUFFER_USAGE_INDEX_BUFFER_BIT
                  
          bdBuffer <$> withDeviceLocalBuffer bufferSize usageFlags indices

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

addNodeMeshToMap
  :: (MeshPrimitive -> SeangineInstance Buffer)
  -> Scene
  -> Map.HashMap MeshPrimitiveId Buffer
  -> Node
  -> SeangineInstance (Map.HashMap MeshPrimitiveId Buffer)
addNodeMeshToMap createBuffer scene buffers node = do
  let mesh = node ^. _nodeMesh scene
  maybe (return buffers) (addMeshPrimitivesToMap createBuffer buffers) mesh

addMeshPrimitivesToMap
  :: (MeshPrimitive -> SeangineInstance Buffer)
  -> Map.HashMap MeshPrimitiveId Buffer
  -> (Int, Mesh)
  -> SeangineInstance (Map.HashMap MeshPrimitiveId Buffer)
addMeshPrimitivesToMap createBuffer buffers (meshId, mesh') = do
  let primitives' = mesh' ^. _meshPrimitives
  V.ifoldM (addMeshPrimitiveToMap createBuffer meshId) buffers primitives'

addMeshPrimitiveToMap createBuffer meshId buffers idx primitive' = do
  let bufferId = MeshPrimitiveId meshId idx
  buffer <- createBuffer primitive'
  return $ Map.insert bufferId buffer buffers

