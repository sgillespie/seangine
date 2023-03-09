module Main (main) where

import Graphics.Seangine.App (App (..), runApp)
import Graphics.Seangine.Lib (projectName)

import Options.Applicative

{-# ANN Options ("HLint: ignore Use newtype instead of data" :: String) #-}
data Options = Options {optVerbose :: !Bool}
  deriving (Show)

main :: IO ()
main = execParser options >>= runApp run

run :: App Options ()
run = do
  opts <- ask
  liftIO $ putTextLn ("Executable for " <> projectName <> ": " <> show opts)

options :: ParserInfo Options
options =
  info (parser <**> helper) $
    fullDesc
      <> progDesc "An experimental 3D rendering engine for Vulkan"
      <> header "seangine - An experimental 3D rendering engine"

parser :: Parser Options
parser = Options <$> verboseOpt
  where
    verboseOpt =
      switch $
        long "verbose"
          <> short 'v'
          <> help "Verbose output?"
