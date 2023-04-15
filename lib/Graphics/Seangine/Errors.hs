module Graphics.Seangine.Errors (SeangineError (..)) where

import Text.Show (Show (show))

data SeangineError
  = ImpossibleError
  | NoDeviceError
  deriving (Typeable)

instance Exception SeangineError

instance Show SeangineError where
  show ImpossibleError = "The impossible happened!"
  show NoDeviceError = "Can't find a suitable device!"
