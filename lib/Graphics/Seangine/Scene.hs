module Graphics.Seangine.Scene
  ( Scene(..),
    _allMeshPrimitives,
  ) where

import Data.Functor.Const
import Lens.Micro
import Prelude
import Text.GLTF.Loader
import qualified Data.Vector as V

type Scene = Gltf

_allMeshPrimitives :: SimpleGetter Gltf (V.Vector MeshPrimitive)
_allMeshPrimitives = to getMeshPrimitives

getMeshPrimitives :: Gltf -> V.Vector MeshPrimitive
getMeshPrimitives scene
  = let nodes = scene ^. _nodes
        meshIds = mapM (^. _nodeMeshId) nodes
        meshes = maybe [] (fmap ((scene ^. _meshes) V.!)) meshIds
    in V.concatMap (^. _meshPrimitives) meshes
