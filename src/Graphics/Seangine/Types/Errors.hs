module Graphics.Seangine.Types.Errors (Errors (..)) where

data Errors
  = WindowSystemFailure
  | ImpossibleError
  deriving (Eq, Show)

instance Exception Errors
