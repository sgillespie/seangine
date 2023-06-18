module Main (main) where

import Data.Version
import Graphics.Seangine (SeangineOptions (..), runSeangine)
import Options.Applicative
import Paths_seangine

data SeangineCli = OptVersion | OptRun SeangineOptions
  deriving (Eq)

main :: IO ()
main = run =<< execParser opts'
  where
    opts' = info (cliParser <**> helper) infoMods
    infoMods = fullDesc <> header "Seangine - A Vulkan GlTF viewer"

run :: SeangineCli -> IO ()
run (OptRun opts) = runSeangine opts
run OptVersion = printVersion

cliParser :: Parser SeangineCli
cliParser = versionParser <|> (OptRun <$> regularParser)

regularParser :: Parser SeangineOptions
regularParser = SeangineOptions <$> fileParser <*> deviceParser <*> debugParser

versionParser :: Parser SeangineCli
versionParser = flag' OptVersion $ long "version" <> short 'V' <> help "Print version"

debugParser :: Parser Bool
debugParser =
  switch $
    long "verbose"
      <> short 'v'
      <> help "Debug logging (requires vulkan-validation-layers)"

fileParser :: Parser FilePath
fileParser = strArgument $ metavar "FILE"

deviceParser :: Parser (Maybe String)
deviceParser = optional $ strOption $ long "device" <> short 'd' <> help "Vulkan device"

printVersion :: IO ()
printVersion = putStrLn $ "Seangine" <> " " <> showVersion version
