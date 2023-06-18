module Main (main) where

import Graphics.Seangine (SeangineCli (..))
import Options.Applicative

main :: IO ()
main = run =<< execParser opts'
  where
    opts' = info (parser <**> helper) infoMods
    infoMods =
      fullDesc
        <> header "Seangine - A Vulkan GlTF viewer"

run :: SeangineCli -> IO ()
run _ = pass

parser :: Parser SeangineCli
parser = SeangineCli <$> file <*> device <*> debug <*> version

debug :: Parser Bool
debug =
  switch $
    long "verbose"
      <> short 'v'
      <> help "Debug logging (requires vulkan-validation-layers)"

version :: Parser Bool
version = switch $ long "version" <> short 'V' <> help "Print version"

file :: Parser FilePath
file = strArgument $ metavar "FILE"

device :: Parser (Maybe String)
device = optional $ strOption $ long "device" <> short 'd' <> help "Vulkan device"
