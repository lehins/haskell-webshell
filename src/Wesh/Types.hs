{-# LANGUAGE RankNTypes #-}
module Wesh.Types where

import Network.WebSockets.Connection
import Data.Conduit
import RIO
import RIO.Process
import System.Posix.Types (Fd)

type WeshState = IORef (Map ByteString Terminal)

data Terminal = Terminal
  { tInputSink  :: !(forall o m . MonadIO m => ConduitT ByteString o m ())
  , tOutputSource :: !(forall i m . MonadIO m => ConduitT i ByteString m ())
  , tFd :: !(IORef (Maybe Fd))
  }

data WeshEnv = WeshEnv
  { weshEnvState   :: !WeshState
  , weshEnvLogFunc :: !LogFunc
  }

data WeshSession = WeshSession
  { weshSessionEnv            :: !WeshEnv
  , weshSessionProcessContext :: !ProcessContext
  , weshSessionConnection     :: !Connection
  }

class HasConnection env where
  connectionG :: SimpleGetter env Connection

instance HasConnection WeshSession where
  connectionG = to weshSessionConnection

instance HasLogFunc WeshEnv where
  logFuncL = lens weshEnvLogFunc (\c f -> c {weshEnvLogFunc = f})

instance HasProcessContext WeshSession where
  processContextL = lens weshSessionProcessContext (\c f -> c {weshSessionProcessContext = f})

instance HasLogFunc WeshSession where
  logFuncL =
    lens
    (\c -> weshSessionEnv c ^. logFuncL)
    (\c lf -> c {weshSessionEnv = set logFuncL lf (weshSessionEnv c)})
