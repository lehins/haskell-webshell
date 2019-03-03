{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module Wesh.Terminal
  ( Terminal(..)
  , withTerminal
  ) where

import Conduit
import Control.Monad.Reader
import Foreign.C.Error
import Foreign.C.Types
import RIO
import RIO.Process
import System.Posix.IO hiding (createPipe)
import System.Posix.Terminal
import System.Posix.Types (Fd)

import Wesh.Types

foreign import ccall "resize" c_resize :: CInt -> CUShort -> CUShort -> IO CInt

withTerminal ::
     (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
  => Text
  -> FilePath
  -> [String]
  -> (Terminal -> m a)
  -> m (Either a ExitCode)
withTerminal token cmd args onTerminal =
  withPseudoTerminal token $ \masterHdl slaveHdl fdRef ->
    proc cmd args $ \processConfig -> do
      let customProcessConfig =
            setCreateGroup True $
            setNewSession True $
            setStdin (useHandleClose slaveHdl) $
            setStdout (useHandleClose slaveHdl) $
            setStderr (useHandleClose slaveHdl) processConfig
      withProcess customProcessConfig $ \process ->
        race
          (onTerminal (Terminal (sinkHandle masterHdl) (sourceHandle masterHdl) fdRef))
          (waitExitCode process)

withPseudoTerminal ::
     MonadUnliftIO m => Text -> (Handle -> Handle -> IORef (Maybe Fd) -> m c) -> m c
withPseudoTerminal token f = bracket openPseudoTerminalHandles closeHandles useHandles
  where
    openPseudoTerminalHandles =
      liftIO $ do
        (masterFd, slaveFd) <- openPseudoTerminal
        masterHdl <- fdToHandle masterFd
        slaveHdl <- fdToHandle slaveFd
        void $
          throwErrnoIfMinus1 "Could not resize the terminal" $
          c_resize (fromIntegral slaveFd) 100 200
        fdRef <- newIORef $ Just slaveFd
        pure (masterHdl, slaveHdl, fdRef)
    closeHandles (masterHdl, slaveHdl, fdRef) =
      liftIO $ writeIORef fdRef Nothing >> hClose masterHdl >> hClose slaveHdl
    useHandles (masterHdl, slaveHdl, fdRef) = f masterHdl slaveHdl fdRef

--resizeTerminal

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
