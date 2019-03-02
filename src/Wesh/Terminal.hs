{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
module Wesh.Terminal
  ( Terminal
  , createTerminal
  , spawnTerminal
  , tPutStr
  , tPutWords
  , tGetStr
  , tGetId
  , tKill
  , terminalSource
  , terminalSink
  ) where


import Conduit
import Control.Monad.Reader
import Network.WebSockets.Connection
import RIO as RIO
import RIO.ByteString as B
import Data.ByteString.Builder
import Yesod (MonadHandler)
import qualified Yesod.WebSockets as WS

import Wesh.Parser
import Wesh.Types



import Foreign.C.Types
import Foreign.Ptr (Ptr, nullPtr)

--import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import System.Exit
import System.Posix.IO
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Terminal
import System.Posix.Types
import System.Posix.Unistd (sleep)

#include <asm/ioctls.h>

data Terminal = KashMVHTerminal (MVar Handle) ProcessID

foreign import ccall "ioctl" c_ioctl :: CInt -> CInt -> Ptr () -> IO CInt
cTIOCSCTTY :: CInt
cTIOCSCTTY = TIOCSCTTY


createTerminal :: FilePath -> [String] -> IO Terminal
createTerminal cmd args = do
  t <- spawnTerminal cmd args
  tid <- tGetId t
  return t

childTerminal :: Fd -> Fd -> FilePath -> [String] -> IO b
childTerminal masterFd slaveFd cmd args = do
  -- The calling process must be a session leader
  createSession
  -- Make  the  given  terminal  the  controlling terminal of the calling process.
  c_ioctl (fromIntegral slaveFd) (fromIntegral cTIOCSCTTY) nullPtr
  -- Duplicate file descriptors to std
  mapM_ (dupTo slaveFd) [stdInput, stdOutput, stdError]
  -- Close File Descriptors
  mapM_ closeFd [slaveFd, masterFd]
  executeFile cmd True args Nothing

childTerminalHandler :: ProcessID -> Fd -> Fd -> IO ()
childTerminalHandler pid masterFd slaveFd = do
  -- Call waitpid.  Neither WNOHANG nor WUNTRACED is set in this call.
  procStat <- getProcessStatus False False pid
  mapM_ closeFd [masterFd, slaveFd]
  case procStat of
    -- Normal exit
    Just (Exited code)  -> exitWith code
    -- Missing child process?
    Nothing             -> exitFailure
    -- Child terminated with some signal
    Just (Terminated _ _) -> exitFailure
    -- Stopped by the debugger - this probably shouldn't happen?  If
    -- it should, we can just return since the handler should still be
    -- active.
    Just (Stopped _)    -> return ()

spawnTerminal :: FilePath -> [String] -> IO Terminal
spawnTerminal cmd args = do
  -- Creates a terminal
  (masterFd, slaveFd) <- openPseudoTerminal
  -- Brings terminal to live
  pid <- forkProcess $ childTerminal masterFd slaveFd cmd args
  -- Replace default hdlr
  installHandler sigCHLD (Catch (childTerminalHandler pid masterFd slaveFd)) Nothing
  -- Conver file descriptor to a normal handle
  hdl <- fdToHandle masterFd
  mvh <- newMVar hdl
  return $ KashMVHTerminal mvh pid


hGetContentsNoClose :: Handle -> IO B.ByteString
hGetContentsNoClose hdl = fmap B.concat (hGetContentsNoCloseAcc hdl [])
  where
    hGetContentsNoCloseAcc h t = do
      ready <- hReady h
      if ready
        then do
          c <- B.hGet h 1
          hGetContentsNoCloseAcc h $ c : t
        else return $ RIO.reverse t

mvhGetContents :: MVar Handle -> IO (Maybe B.ByteString)
mvhGetContents mvh = do
  hdl <- takeMVar mvh
  isReady <- hReady hdl
  if isReady
    then do
      content <- hGetContentsNoClose hdl
      putMVar mvh hdl
      --logDebug $ "mvhGetContents: " <> displayBytesUtf8 content
      return $ Just content
    else do
      putMVar mvh hdl
      return Nothing

tGetStr :: Terminal -> IO (Maybe B.ByteString)
tGetStr (KashMVHTerminal mvh _) = mvhGetContents mvh

terminalSource :: MonadIO m => Terminal -> ConduitT i ByteString m ()
terminalSource (KashMVHTerminal mvh _) = do
  hdl <- readMVar mvh
  sourceHandle hdl

terminalSink :: MonadIO m => Terminal -> ConduitT ByteString o m ()
terminalSink (KashMVHTerminal mvh _) = do
  hdl <- readMVar mvh
  sinkHandle hdl

tPutStr :: Terminal -> ByteString -> IO ()
tPutStr (KashMVHTerminal mvh _) st = do
  hdl <- takeMVar mvh
  RIO.hPutBuilder hdl (byteString st) >> hFlush hdl
  putMVar mvh hdl

tPutWords :: Terminal -> [Word8] -> IO ()
tPutWords (KashMVHTerminal mvh _) ws = do
  hdl <- takeMVar mvh
  mapM_ (RIO.hPutBuilder hdl . word8) ws
  putMVar mvh hdl

tGetId :: Terminal -> IO Int
tGetId (KashMVHTerminal _ pid) = return $ fromIntegral pid

tKill :: Terminal -> IO ()
tKill (KashMVHTerminal _ tid) = signalProcess killProcess tid

-- printer' mvh = do
--   hdl <- takeMVar mvh
--   isReady <- hReady hdl
--   if isReady
--     then do
--       content <- hGetContentsNoClose hdl
--       putMVar mvh hdl
--     else putMVar mvh hdl
--   threadDelay 500000
--   printer' mvh


-- main = do
--   (KashMVHTerminal mvth _) <- spawnTerminal "/bin/bash" []
--   async $ printer' mvth
--   let input = do
--         RIO.hPutBuilder stdout "in input."
--         threadDelay 1000000
--         cmd <- B.hGetLine stdin
--         if not $ B.null cmd then do
--           hin <- takeMVar mvth
--           B.hPut hin cmd >> hFlush hin
--           putMVar mvth hin
--           else return ()
--         input
--   input
--   return ()
