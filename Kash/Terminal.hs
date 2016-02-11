{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Kash.Terminal (
  Terminal, 
  spawnTerminal, 
  tPutStr, 
  tPutWords, 
  tGetStr, 
  tGetId, 
  tKill) where

import Foreign.C.Types
import Foreign.Ptr (Ptr, nullPtr)

import System.Posix.IO
import System.Posix.Terminal
import System.Posix.Process
import System.Posix.Types
import System.Posix.Signals
import System.Posix.Unistd (sleep)
import System.IO
import System.Exit
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8

#include <asm/ioctls.h>

data Terminal = KashMVHTerminal (MVar Handle) ProcessID |
                KashHTerminal Handle |
                KashFdTerminal Fd

foreign import ccall "ioctl" c_ioctl :: CInt -> CInt -> Ptr () -> IO CInt
cTIOCSCTTY :: CInt
cTIOCSCTTY = TIOCSCTTY
  
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
    Just (Exited code) -> exitWith code
    -- Missing child process?
    Nothing -> exitFailure
    -- Child terminated with some signal
    Just (Terminated _) -> exitFailure
    -- Stopped by the debugger - this probably shouldn't happen?  If
    -- it should, we can just return since the handler should still be
    -- active.
    Just (Stopped _) -> return ()
  
spawnTerminal :: FilePath -> [String] -> IO Terminal
spawnTerminal cmd args = do
  -- Creates a terminal
  (masterFd, slaveFd) <- openPseudoTerminal
  -- Brings terminal to live
  pid <- forkProcess $ childTerminal masterFd slaveFd cmd args
  -- Replace default handler
  installHandler sigCHLD (Catch (childTerminalHandler pid masterFd slaveFd)) Nothing
  -- Conver file descriptor to a normal handle
  handle <- fdToHandle masterFd
  mvh <- newMVar handle
  return $ KashMVHTerminal mvh pid

  
hGetContentsNoClose :: Handle -> IO B.ByteString
hGetContentsNoClose handle = fmap BC8.pack (hGetContentsNoCloseAcc handle "") where
  hGetContentsNoCloseAcc h t = do
    ready <- hReady h
    if ready then do
      c <- hGetChar h
      hGetContentsNoCloseAcc h $ c:t
      else return $ reverse t

mvhGetContents :: MVar Handle -> IO (Maybe B.ByteString)
mvhGetContents mvh = do
  handle <- takeMVar mvh
  isReady <- hReady handle
  if isReady then do
    content <- hGetContentsNoClose handle
    putMVar mvh handle
    B.putStr content
    return $ Just content
    else do
    putMVar mvh handle
    return $ Nothing

tGetStr :: Terminal -> IO (Maybe B.ByteString)
tGetStr (KashMVHTerminal mvh _) = mvhGetContents mvh

tPutStr :: Terminal -> String -> IO ()
tPutStr (KashMVHTerminal mvh _) st = do
  handle <- takeMVar mvh
  hPutStr handle st >> hFlush handle
  putMVar mvh handle

tPutWords :: Terminal -> [Word8] -> IO ()
tPutWords (KashMVHTerminal mvh _) ws = do
  handle <- takeMVar mvh
  --B.putStr $ B.pack ws
  B.hPutStr handle $ B.pack ws
  putMVar mvh handle

tGetId :: Terminal -> IO Int
tGetId (KashMVHTerminal _ pid) = return $ fromIntegral pid

tKill :: Terminal -> IO ()
tKill (KashMVHTerminal _ tid) = signalProcess killProcess tid

printer' mvh = do
  let hGetContentsNoClose h s = do 
        ready <- hReady h
        if ready then do
          c <- hGetChar h
          hGetContentsNoClose h $ s++[c]
          else return s
  handle <- takeMVar mvh
  isReady <- hReady handle
  if isReady then do
    content <- hGetContentsNoClose handle ""
    putMVar mvh handle
    else putMVar mvh handle
  threadDelay 500000
  printer' mvh


main = do
  (KashMVHTerminal mvth _) <- spawnTerminal "/bin/login" []
  forkIO $ printer' mvth
  let input = do
        putStrLn "in input."
        threadDelay 1000000
        cmd <- getLine
        if not $ null cmd then do
          hin <- takeMVar mvth
          hPutStrLn hin cmd >> hFlush hin
          putMVar mvth hin
          else return ()
        input
  input
  return ()