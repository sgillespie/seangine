module Graphics.Seangine.App (App (..), runApp) where

import UnliftIO

newtype App env a = App {unApp :: ReaderT env IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader env,
      MonadIO,
      MonadUnliftIO
    )

runApp :: MonadIO io => App env a -> env -> io a
runApp act env = liftIO $ runReaderT (unApp act) env
