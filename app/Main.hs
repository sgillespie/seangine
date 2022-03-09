module Main where

import Graphics.Seangine
import Graphics.Seangine.Monad (runVulkan)
import Paths_seangine (getDataDir)

import Control.Monad.Extra (anyM, whileM)
import Control.Monad.Trans.Resource

import Control.Monad.IO.Class

main :: IO ()
main = runResourceT $ do
  dataDir <- liftIO getDataDir
  (_, win) <- withWindow "Seangine 0.1.1.0" 800 600
  windowExts <- windowExts win
  instance' <- withInstance' windowExts
  (_, surface) <- withWindowSurface instance' win
  handles <- withVulkanInstance dataDir instance' surface

  runVulkan handles $ do
    whileM (not . shouldQuit <$> awaitWindowEvents)
