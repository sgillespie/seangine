module Graphics.Seangine.Errors (SeangineError (..)) where

import Text.Show (Show (show))

data SeangineError = ImpossibleError
  deriving (Typeable)

instance Exception SeangineError

instance Show SeangineError where
  show ImpossibleError = "The impossible happened!"
