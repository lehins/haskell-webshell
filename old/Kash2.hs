{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Kash (spawnTerminal) where

--import Foreign
--import Foreign.C.Error
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


#include <asm/ioctls.h>


getAllContent h acc = do 
  nl <- hGetLine h
  let nacc = acc++nl
  isReady <- hReady h
  if isReady then getAllContent h nacc else return nacc
  
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

childTerminalHandler :: ProcessID -> IO ()
childTerminalHandler pid = do
  -- Call waitpid.  Neither WNOHANG nor WUNTRACED is set in this call.
  procStat <- getProcessStatus False False pid
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
  
spawnTerminal :: FilePath -> [String] -> IO Handle
spawnTerminal cmd args = do
  -- Creates a terminal
  (masterFd, slaveFd) <- openPseudoTerminal
  -- Brings terminal to live
  pid <- forkProcess $ childTerminal masterFd slaveFd cmd args
  -- Replace default handler
  installHandler sigCHLD (Catch (childTerminalHandler pid)) Nothing
  handle <- fdToHandle masterFd
  return handle

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
    putStrLn ("received out: " ++ content)
    putMVar mvh handle
    else putMVar mvh handle
  threadDelay 500000
  printer' mvh


main = do
  th <- spawnTerminal "/bin/login" []
  mvth <- newMVar th
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