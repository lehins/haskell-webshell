{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Kash (spawnTerminal) where

import Foreign
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Ptr

import System.Posix.IO
import System.Posix.Terminal
import System.Posix.Process
import System.Posix.Types
import System.Posix.Unistd (sleep)
import System.IO
import Control.Concurrent
import Control.Concurrent.MVar


#include <asm/ioctls.h>
 
foreign import ccall "ioctl" c_ioctl :: CInt -> CInt -> Ptr () -> IO CInt
cTIOCSCTTY :: CInt
cTIOCSCTTY = TIOCSCTTY
  
childTerminal masterFd slaveFd cmd args = do
  -- The calling process must be a session leader
  createSession
  -- Make  the  given  terminal  the  controlling terminal of the calling process.
  c_ioctl (fromIntegral slaveFd) (fromIntegral cTIOCSCTTY) nullPtr
  -- Duplicate file descriptors to std
  --mapM_ (dupTo slaveFd) [stdInput, stdOutput, stdError]
  dupTo slaveFd stdInput
  dupTo slaveFd stdOutput
  dupTo slaveFd stdError
  -- Close File Descriptors
  mapM_ closeFd [slaveFd, masterFd]
  executeFile cmd True args Nothing

spawnTerminal :: IO Handle
spawnTerminal = do
  (masterFd, slaveFd) <- openPseudoTerminal
  pid <- forkProcess (childTerminal masterFd slaveFd "/bin/login" [])
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
  th <- spawnTerminal 
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