{-# LANGUAGE UndecidableInstances #-}

module Graphics.Seangine.Config
  ( Config (..),
    Options (..),
    HasConfig (..),
    HasOptions (..),
  ) where

import Graphics.Seangine.Config.VulkanHandles

data Config = Config
  { cfgVulkanHandles :: VulkanHandles,
    cfgOptions :: Options
  }

data Options = Options
  { optVerbose :: Bool,
    optFile :: FilePath
  }

class HasConfig m where
  getVulkanHandles :: m VulkanHandles
  getOptions :: m Options

class HasOptions m where
  getOptVerbose :: m Bool
  getOptFile :: m FilePath
