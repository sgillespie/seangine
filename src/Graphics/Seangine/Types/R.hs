module Graphics.Seangine.Types.R
  ( R (..),
    runR,
  ) where

import Graphics.Seangine.Types.Config

import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Trans.Resource

newtype R a = R {unR :: ReaderT SeangineConfig (ResourceT IO) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader SeangineConfig,
      MonadResource,
      MonadIO,
      MonadFail
    )

instance MonadUnliftIO R where
  withRunInIO :: ((forall a. R a -> IO a) -> IO b) -> R b
  withRunInIO a = R $ withRunInIO (\r -> a $ r . unR)

runR :: SeangineConfig -> R a -> ResourceT IO a
runR config = usingReaderT config . unR
