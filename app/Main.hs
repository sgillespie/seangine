module Main where

import Graphics.Seangine

import Control.Monad.Extra (anyM, whileM)
import Control.Monad.Trans.Resource

import Control.Monad.IO.Class

main :: IO ()
main = runResourceT $ do
  (_, win) <- withWindow "Seangine 0.1.1.0" 800 600
  windowExts <- windowExts win
  instance' <- withInstance' windowExts
  (_, surface) <- withWindowSurface instance' win

  whileM (not . shouldQuit <$> awaitWindowEvents)
