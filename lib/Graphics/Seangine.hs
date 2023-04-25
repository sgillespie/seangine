module Graphics.Seangine
  ( module Graphics.Seangine.Config,
    module Graphics.Seangine.Config.Frame,
    module Graphics.Seangine.Config.VulkanHandles,
    module Graphics.Seangine.Errors,
    module Graphics.Seangine.Instance,
    module Graphics.Seangine.Monad,
    module Graphics.Seangine.Scene,
    module Graphics.Seangine.Window,
    runSeangine,
    sdlWindowSystem,
  ) where

import Graphics.Seangine.Config
import Graphics.Seangine.Config.Frame
import Graphics.Seangine.Config.VulkanHandles
import Graphics.Seangine.Errors
import Graphics.Seangine.Instance
import Graphics.Seangine.Monad
import Graphics.Seangine.Scene
import Graphics.Seangine.Window
import Graphics.Seangine.Window.SDL

import Control.Monad.Trans.Resource (MonadUnliftIO (), runResourceT)
import System.FilePath ((</>))
import Text.GLTF.Loader hiding (ImpossibleError)
import UnliftIO.Exception (throwIO)

runSeangine :: Options -> FilePath -> IO ()
runSeangine opts dataDir = runResourceT $ do
  scene <- loadScene $ dataDir </> optFile opts

  (_, win) <- withWindow sdlWindowSystem "Seangine 0.1.1.0" 800 600
  windowExts <- getVulkanExtensions win
  instance' <- withVulkanInstance windowExts
  (_, surface) <- withWindowSurface instance' win
  handles <- withVulkanHandles dataDir instance' surface

  let config =
        Config
          { cfgVulkanHandles = handles,
            cfgOptions = opts
          }
  runVulkan config $ do
    pass

loadScene :: MonadUnliftIO io => FilePath -> io Scene
loadScene path = do
  result <- fromJsonFile path
  case result of
    Left _ -> throwIO ImpossibleError
    Right scene -> return scene
