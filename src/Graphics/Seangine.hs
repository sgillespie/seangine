module Graphics.Seangine (SeangineCli (..)) where

data SeangineCli = SeangineCli
  { optFile :: !FilePath,
    optDevice :: !(Maybe String),
    optDebug :: !Bool,
    optVersion :: !Bool
  }
