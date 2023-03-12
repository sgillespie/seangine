module Graphics.Seangine.Scene
  ( Scene (),
    MeshPrimitiveId (..),
    _allMeshPrimitives,
    _nodeMesh,
  ) where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Lens.Micro
import Text.GLTF.Loader

type Scene = Gltf

-- | Uniquely identifies a MeshPrimitive
data MeshPrimitiveId = MeshPrimitiveId
  { meshId :: Int,
    meshPrimitiveId :: Int
  }
  deriving (Eq, Generic, Show)

instance Hashable MeshPrimitiveId

_allMeshPrimitives :: SimpleGetter Gltf (Vector MeshPrimitive)
_allMeshPrimitives = to getMeshPrimitives

_nodeMesh :: Gltf -> SimpleGetter Node (Maybe (Int, Mesh))
_nodeMesh scene = to $ \node -> do
  meshId <- node ^. _nodeMeshId
  mesh <- scene ^. _meshes . to (Vector.!? meshId)
  return (meshId, mesh)

getMeshPrimitives :: Gltf -> Vector MeshPrimitive
getMeshPrimitives scene = Vector.concatMap (^. _meshPrimitives) meshes
  where
    meshes = maybe [] (fmap ((scene ^. _meshes) Vector.!)) meshIds
    meshIds :: Maybe (Vector Int)
    meshIds = mapM (^. _nodeMeshId) nodes
    nodes = scene ^. _nodes
