module Main (main) where

import Graphics.Seangine hiding (getDataDir)
import Paths_seangine (getDataDir)

import Options.Applicative
import System.FilePath ((</>))

main :: IO ()
main = execParser options >>= run

run :: Options -> IO ()
run opts = getDataDir >>= runSeangine opts

options :: ParserInfo Options
options =
  info (parser <**> helper) $
    fullDesc
      <> progDesc "An experimental 3D rendering engine for Vulkan"
      <> header "seangine - An experimental 3D rendering engine"

parser :: Parser Options
parser = Options <$> verboseOpt <*> fileOpt
  where
    verboseOpt =
      switch $
        long "verbose"
          <> short 'v'
          <> help "Verbose output?"

    fileOpt =
      strOption $
        long "file"
          <> short 'f'
          <> help "File?"
          <> value ("data" </> "cube.gltf")
