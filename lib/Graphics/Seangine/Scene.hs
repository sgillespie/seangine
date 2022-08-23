module Graphics.Seangine.Scene
  ( Scene(..),
    _allMeshPrimitives,
  ) where

import Data.Functor.Const
import Lens.Micro
import Text.GLTF.Loader

type Scene = Gltf

_allMeshPrimitives :: SimpleGetter Gltf [MeshPrimitive]
_allMeshPrimitives = to getMeshPrimitives

getMeshPrimitives scene
  = let nodes = scene ^. _nodes
        meshIds = mapM (^. _nodeMeshId) nodes
        meshes = maybe [] (map ((scene ^. _meshes) !!)) meshIds
    in concatMap (^. _meshPrimitives) meshes
