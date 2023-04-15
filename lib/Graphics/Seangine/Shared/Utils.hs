module Graphics.Seangine.Shared.Utils
  ( fromResult,
    oneSecond,
  ) where

import Vulkan.Core10.Enums.Result (Result (..))

fromResult :: Monoid m => (Result, m) -> m
fromResult (SUCCESS, ls) = ls
fromResult _ = mempty

oneSecond :: Fractional a => a
oneSecond = 1e9
