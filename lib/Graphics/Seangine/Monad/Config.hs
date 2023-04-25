module Graphics.Seangine.Monad.Config (Config (..), Options (..)) where

import Graphics.Seangine.Monad.Vulkan (VulkanHandles (..))

data Config = Config
  { cfgOptions :: Options,
    cfgVulkanHandles :: VulkanHandles
  }

data Options = Options
  {optVerbose :: Bool}
