module Main (main) where

import System.Process (system)

main :: IO ()
main = system "fourmolu --mode check app lib test scripts" >>= exitWith
