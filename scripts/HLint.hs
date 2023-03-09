module Main (main) where

import Language.Haskell.HLint (hlint)

arguments :: [String]
arguments =
  [ "app",
    "scripts",
    "lib",
    "test"
  ]

main :: IO ()
main =
  hlint arguments >>= \case
    [] -> exitSuccess
    _ -> exitFailure
