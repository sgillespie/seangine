module Main where

import Graphics.Seangine
import Graphics.Seangine.VulkanFrame (withVulkanFrame)
import Graphics.Seangine.Monad (runVulkan)
import Graphics.Seangine.Window.SDL
import Paths_seangine (getDataDir)

import Control.Monad.Extra (anyM, whileM)
import Control.Monad.Trans.Resource

import Control.Monad.IO.Class

main :: IO ()
main = runResourceT $ do
  dataDir <- liftIO getDataDir
  (_, win) <- withWindow sdlWindowSystem "Seangine 0.1.1.0" 800 600
  windowExts <- getVulkanExtensions win
  instance' <- withInstance' windowExts
  (_, surface) <- withWindowSurface instance' win
  handles <- withVulkanInstance dataDir instance' surface

  runVulkan handles $ do
    initialFrame <- withVulkanFrame win surface
    
    whileM (not . shouldQuit <$> awaitWindowEvents win)
