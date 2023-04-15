module Main (main) where

import Graphics.Seangine hiding (getDataDir)
import Graphics.Seangine.App (App (..), runApp)
import Paths_seangine (getDataDir)

import Control.Monad.Trans.Resource
import Options.Applicative
import System.FilePath ((</>))
import Text.GLTF.Loader hiding (ImpossibleError)
import UnliftIO.Exception

{-# ANN Options ("HLint: ignore Use newtype instead of data" :: String) #-}
data Options = Options {optVerbose :: !Bool}
  deriving (Show)

main :: IO ()
main = execParser options >>= runApp run

run :: App Options ()
run = runResourceT $ do
  _ <- ask
  dataDir <- liftIO getDataDir
  scene <- loadScene $ dataDir </> "data" </> "cube.gltf"
  (_, win) <- withWindow sdlWindowSystem "Seangine 0.1.1.0" 800 600
  windowExts <- getVulkanExtensions win
  instance' <- withVulkanInstance windowExts
  (_, surface) <- withWindowSurface instance' win
  handles <- withVulkanHandles dataDir instance' surface

  pass

loadScene :: MonadUnliftIO io => FilePath -> io Scene
loadScene path = do
  result <- fromJsonFile path
  case result of
    Left _ -> throwIO ImpossibleError
    Right scene -> return scene

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
