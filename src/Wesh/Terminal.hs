{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Wesh.Terminal
  ( Terminal(..)
  , withShell
  , resizeTerminal
  , terminalInputSink
  , terminalOutputSource
  ) where

import Conduit (ConduitT, sinkHandle, sourceHandle)
import Control.Monad.Reader
import Foreign.C.Error
import Foreign.C.Types
import RIO
import RIO.Map as Map
import RIO.Process
import System.Posix.IO (fdToHandle)
import System.Posix.Terminal (openPseudoTerminal)

import Wesh.Types

foreign import ccall "resize" c_resize :: CInt -> CUShort -> CUShort -> IO CInt

withShell ::
     ( MonadReader env m
     , MonadUnliftIO m
     , HasLogFunc env
     , HasProcessContext env
     , HasWeshState env
     )
  => Token -- ^ Opaque identifier for the terminal session
  -> FilePath -- ^ Name of shell
  -> [String] -- ^ Shell arguments
  -> (Terminal -> m a) -- ^ Action to execute on the terminal that runs the shell
  -> m (Either a ExitCode)
withShell token cmd args onTerminal =
  withPseudoTerminal token $ \terminal@Terminal {tSlaveHandle} ->
    proc cmd args $ \processConfig ->
      let customProcessConfig =
            setCreateGroup True $
            setNewSession True $
            setStdin (useHandleClose tSlaveHandle) $
            setStdout (useHandleClose tSlaveHandle) $
            setStderr (useHandleClose tSlaveHandle) processConfig
       in withProcess customProcessConfig $ \process ->
            race (onTerminal terminal) (waitExitCode process)

terminalInputSink :: MonadIO m => Terminal -> ConduitT ByteString o m ()
terminalInputSink = sinkHandle . tMasterHandle

terminalOutputSource :: MonadIO m => Terminal -> ConduitT i ByteString m ()
terminalOutputSource = sourceHandle . tMasterHandle

withPseudoTerminal ::
     (MonadReader env m, MonadUnliftIO m, HasWeshState env)
  => Token
  -> (Terminal -> m b)
  -> m b
withPseudoTerminal token onPseudoTerminal = do
  stateRef <- view weshStateG
  let openPseudoTerminalHandles = do
        terminal <- liftIO makePseudoTerminal
        atomicModifyIORef' stateRef $ \state -> (Map.insert token terminal state, ())
        pure terminal
      closeHandles Terminal {tMasterHandle, tSlaveHandle} = do
        atomicModifyIORef' stateRef $ \state -> (Map.delete token state, ())
        liftIO $ hClose tMasterHandle >> hClose tSlaveHandle
  bracket openPseudoTerminalHandles closeHandles onPseudoTerminal

makePseudoTerminal :: IO Terminal
makePseudoTerminal = do
  (masterFd, slaveFd) <- openPseudoTerminal
  masterHdl <- fdToHandle masterFd
  slaveHdl <- fdToHandle slaveFd
  pure $
    Terminal {tMasterHandle = masterHdl, tSlaveHandle = slaveHdl, tFd = slaveFd}

resizeTerminal ::
     (MonadReader env m, MonadIO m, HasWeshState env, HasLogFunc env)
  => Token -- ^ Token that identifies the terminal session
  -> TerminalSize -- ^ New size for the terminal window
  -> m Bool
resizeTerminal token tSize@TerminalSize {tsWidth, tsHeight} = do
  stateRef <- view weshStateG
  state <- readIORef stateRef
  case Map.lookup token state of
    Just Terminal{tFd} -> do
        ioctlResize (fromIntegral tFd) (fromIntegral tsWidth) (fromIntegral tsHeight)
        logDebug $ "Changed the size of the terminal to: " <> display tSize
        pure True
    Nothing -> pure False


-- TODO: implement lenient resizing with proper error logging
ioctlResize :: MonadIO m => CInt -> CUShort -> CUShort -> m ()
ioctlResize fd width height =
  void $
  liftIO $
  throwErrnoIfMinus1 "Could not resize the terminal" $ c_resize fd width height

