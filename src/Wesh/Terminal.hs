{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Wesh.Terminal
  ( Terminal(..)
  , withTerminal
  , resizeTerminal
  ) where

import Conduit (sinkHandle, sourceHandle)
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

withTerminal ::
     ( MonadReader env m
     , MonadUnliftIO m
     , HasLogFunc env
     , HasProcessContext env
     , HasWeshState env
     )
  => Token
  -> FilePath
  -> [String]
  -> (Terminal -> m a)
  -> m (Either a ExitCode)
withTerminal token cmd args onTerminal =
  withPseudoTerminal token $ \terminal@Terminal {tHandle} ->
    proc cmd args $ \processConfig -> do
      let customProcessConfig =
            setCreateGroup True $
            setNewSession True $
            setStdin (useHandleClose tHandle) $
            setStdout (useHandleClose tHandle) $
            setStderr (useHandleClose tHandle) processConfig
      withProcess customProcessConfig $ \process ->
        race (onTerminal terminal) (waitExitCode process)

withPseudoTerminal ::
     (MonadReader env m, MonadUnliftIO m, HasWeshState env)
  => Token
  -> (Terminal -> m b)
  -> m b
withPseudoTerminal token f = do
  stateRef <- view weshStateG
  let openPseudoTerminalHandles = do
        (masterFd, slaveFd) <- liftIO openPseudoTerminal
        masterHdl <- liftIO $ fdToHandle masterFd
        slaveHdl <- liftIO $ fdToHandle slaveFd
        let terminal =
              Terminal
                { tInputSink = sinkHandle masterHdl
                , tOutputSource = sourceHandle masterHdl
                , tHandle = slaveHdl
                , tFd = slaveFd
                }
        atomicModifyIORef' stateRef $ \state -> (Map.insert token terminal state, ())
        pure (masterHdl, terminal)
      closeHandles (masterHdl, Terminal {tHandle}) = do
        atomicModifyIORef' stateRef $ \state -> (Map.delete token state, ())
        liftIO $ hClose masterHdl >> hClose tHandle
  bracket openPseudoTerminalHandles closeHandles (f . snd)


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
        ictlResize (fromIntegral tFd) (fromIntegral tsWidth) (fromIntegral tsHeight)
        logDebug $ "Changed the size of the terminal to: " <> display tSize
        pure True
    Nothing -> pure False


-- TODO: implement lenient resizing with proper error logging
ictlResize :: MonadIO m => CInt -> CUShort -> CUShort -> m ()
ictlResize fd width height =
  void $
  liftIO $
  throwErrnoIfMinus1 "Could not resize the terminal" $ c_resize fd width height
