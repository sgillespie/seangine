module Seangine.Monad.Cmd (MonadCmd(..), CmdT(..), runCmdT) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Resource
import Vulkan.CStruct.Extends
import Vulkan.Core10

import Seangine.Monad.Vulkan

-- |Vulkan command monad class. Wraps a command buffer.
class Monad m => MonadCmd m where
  getCommandBuffer :: m CommandBuffer

instance MonadCmd m => MonadCmd (ReaderT r m) where
  getCommandBuffer = lift getCommandBuffer

-- |A Vulkan command transformer. Executes commands in a fully
-- initialized command buffer, and ends recording upon exiting
newtype CmdT m a = CmdT { unCmdT :: ReaderT CommandBuffer m a }
  deriving newtype (Functor,
                    Applicative,
                    Monad,
                    MonadIO,
                    MonadResource,
                    MonadVulkan
                   )

instance Monad m => MonadCmd (CmdT m) where
  getCommandBuffer = CmdT ask

-- |Record the given commands into the given command buffer. Correctly initializes
-- and destroys the command buffer recording environment. Note: does not destroy
-- the actual command buffer--must be run in a `withCommandBuffer` call
runCmdT
  :: (Extendss CommandBufferBeginInfo a, PokeChain a, MonadIO m, MonadUnliftIO m)
  => CommandBuffer
  -> CommandBufferBeginInfo a
  -> CmdT m r
  -> m r
runCmdT commandBuffer beginInfo (CmdT a)
  = useCommandBuffer commandBuffer beginInfo (runReaderT a commandBuffer)

instance MonadUnliftIO m => MonadUnliftIO (CmdT m) where
  withRunInIO a = CmdT $ withRunInIO (\r -> a (r . unCmdT))
