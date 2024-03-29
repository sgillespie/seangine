module Graphics.Seangine
  ( SeangineOptions (..),
    runSeangine,
  ) where

data SeangineOptions = SeangineOptions
  { optFile :: !FilePath,
    optDevice :: !(Maybe String),
    optDebug :: !Bool
  }
  deriving (Eq)

runSeangine :: SeangineOptions -> IO ()
runSeangine _ = pass
