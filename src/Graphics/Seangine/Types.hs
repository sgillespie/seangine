module Graphics.Seangine.Types (SeangineOptions (..)) where

data SeangineOptions = SeangineOptions
  { optFile :: !FilePath,
    optDevice :: !(Maybe String),
    optDebug :: !Bool
  }
  deriving (Eq)
