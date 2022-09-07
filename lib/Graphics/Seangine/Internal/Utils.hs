module Graphics.Seangine.Internal.Utils
  (mresult,
   throwIfUnsuccessful,
   oneSecond
  ) where

import Graphics.Seangine.Monad.Exception (throwSystemError)

import Prelude
import Control.Monad.IO.Unlift (MonadIO())
import Vulkan.Core10.Enums.Result (Result(..))

mresult :: Monoid m => (Result, m) -> m
mresult (SUCCESS, ls) = ls
mresult _ = mempty

throwIfUnsuccessful :: MonadIO m => String -> Result -> m ()
throwIfUnsuccessful _ SUCCESS = return ()
throwIfUnsuccessful errMessage _ = throwSystemError errMessage

oneSecond :: Num a => a
oneSecond = 1e9
