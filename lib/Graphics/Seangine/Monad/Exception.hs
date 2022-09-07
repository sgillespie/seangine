module Graphics.Seangine.Monad.Exception where

import Control.Monad.IO.Unlift (MonadIO(..))

import Control.Exception
import GHC.IO.Exception (IOErrorType(..))
import Prelude
import System.IO.Error (mkIOError)

systemError :: String -> IOError
systemError str = mkIOError SystemError str Nothing Nothing

throwSystemErrorIo :: String -> IO a
throwSystemErrorIo = throwIO . systemError

throwSystemError :: MonadIO m => String -> m a
throwSystemError = liftIO . throwSystemErrorIo
