module Graphics.Seangine.Scene
  ( Scene(..),
    MeshPrimitiveId(..),
    _allMeshPrimitives,
    _nodeMesh
  ) where

import Control.Monad
import Data.Functor.Const
import Data.Hashable
import Data.HashMap.Strict
import GHC.Generics (Generic)
import Lens.Micro
import Prelude
import Text.GLTF.Loader
import qualified Data.Vector as V

type Scene = Gltf

-- | Uniquely identifies a MeshPrimitive
data MeshPrimitiveId = MeshPrimitiveId
  { meshId :: Int,
    meshPrimitiveId :: Int
  } deriving (Eq, Generic, Show)

instance Hashable MeshPrimitiveId

_allMeshPrimitives :: SimpleGetter Gltf (V.Vector MeshPrimitive)
_allMeshPrimitives = to getMeshPrimitives

_nodeMesh :: Gltf -> SimpleGetter Node (Maybe (Int, Mesh))
_nodeMesh scene = to $ \node -> do
  meshId <- node ^. _nodeMeshId
  mesh <- scene ^. _meshes . to (V.!? meshId)
  return (meshId, mesh)

getMeshPrimitives :: Gltf -> V.Vector MeshPrimitive
getMeshPrimitives scene
  = let nodes = scene ^. _nodes
        meshIds = mapM (^. _nodeMeshId) nodes
        meshes = maybe [] (fmap ((scene ^. _meshes) V.!)) meshIds
    in V.concatMap (^. _meshPrimitives) meshes
