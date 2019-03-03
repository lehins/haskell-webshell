{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module Wesh.Terminal
  ( Terminal(..)
  , withTerminal
  , resizeTerminal
  ) where

import Conduit
import Control.Monad.Reader
import Foreign.C.Error
import Foreign.C.Types
import RIO
import RIO.Map as Map
import RIO.Process
import System.Posix.IO hiding (createPipe)
import System.Posix.Terminal

import Wesh.Types

foreign import ccall "resize" c_resize :: CInt -> CUShort -> CUShort -> IO CInt

withTerminal ::
     Token
  -> FilePath
  -> [String]
  -> (Terminal -> RIO WeshSession a)
  -> RIO WeshSession (Either a ExitCode)
withTerminal token cmd args onTerminal =
  withPseudoTerminal token $ \terminal@Terminal {tHandle} ->
    proc cmd args $ \processConfig -> do
      let customProcessConfig =
            setCreateGroup True $
            setNewSession True $
            setStdin (useHandleClose tHandle) $
            setStdout (useHandleClose tHandle) $
            setStderr (useHandleClose tHandle) processConfig
      logInfo "Starting new terminal"
      withProcess customProcessConfig $ \process ->
        race (onTerminal terminal) (waitExitCode process)

withPseudoTerminal :: Token -> (Terminal -> RIO WeshSession c) -> RIO WeshSession c
withPseudoTerminal token f = do
  weshEnv <- asks weshSessionEnv
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
        atomicModifyIORef' (weshEnvState weshEnv) $ \state ->
          (Map.insert token terminal state, ())
        pure (masterHdl, terminal)
      closeHandles (masterHdl, Terminal {tHandle}) = do
        atomicModifyIORef' (weshEnvState weshEnv) $ \state ->
          (Map.delete token state, ())
        liftIO $ hClose masterHdl >> hClose tHandle
      useHandles (_, terminal) = do
        -- void $
        --   liftIO $
        --   throwErrnoIfMinus1 "Could not resize the terminal" $
        --   c_resize (fromIntegral (tFd terminal)) 100 200
        f terminal
  bracket openPseudoTerminalHandles closeHandles useHandles

resizeTerminal :: Token -> TerminalSize -> RIO WeshEnv Bool
resizeTerminal token tSize@TerminalSize {tsWidth, tsHeight} = do
  stateRef <- asks weshEnvState
  state <- readIORef stateRef
  -- TODO: implement lenient resising with error logging
  case Map.lookup token state of
    Just Terminal{tFd} -> do
        void $
          liftIO $
          throwErrnoIfMinus1 "Could not resize the terminal" $
          c_resize (fromIntegral tFd) (fromIntegral tsWidth) (fromIntegral tsHeight)
        logDebug $ "Changed the size of the terminal to: " <> display tSize
        pure True
    Nothing -> pure False

getErrnoTxt :: MonadIO m => m Utf8Builder
getErrnoTxt = toTextErrno <$> liftIO getErrno
  where
    toTextErrno errno@(Errno ec)
      | errno == eBADF = "fd is not a valid file descriptor"
      | errno == eFAULT = "argp references an inaccessible memory area."
      | errno == eINVAL = "request or argp is not valid."
      | errno == eNOTTY =
        "fd is not associated with a character special device."
      | errno == eNOTTY =
        "The specified request does not apply to the kind of object\
        \that the file descriptor fd references."
      | otherwise = "Unexpected errno: " <> displayShow ec
