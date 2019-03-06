{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Wesh.Types where

import Data.Aeson
import Network.WebSockets.Connection
import RIO
import RIO.Process
import System.Posix.Types (Fd)
import Yesod (PathPiece)

newtype Token = Token Text deriving (Eq, Ord, Show, Read, PathPiece)

type WeshState = IORef (Map Token Terminal)

data Terminal = Terminal
  { tMasterHandle :: !Handle
  , tSlaveHandle  :: !Handle
  , tFd           :: !Fd
  }


data TerminalSize = TerminalSize
  { tsWidth  :: !Word16
  , tsHeight :: !Word16
  } deriving Show

instance Display TerminalSize where
  display TerminalSize {tsWidth, tsHeight} =
    "width=" <> displayShow tsWidth <> ",height=" <> displayShow tsHeight

instance FromJSON TerminalSize where
  parseJSON =
    withObject "TerminalSize" $ \o -> do
      tsWidth <- o .: "width"
      tsHeight <- o .: "height"
      pure
        TerminalSize{tsWidth, tsHeight}


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

class HasWeshState env where
  weshStateG :: SimpleGetter env WeshState

instance HasWeshState WeshEnv where
  weshStateG = to weshEnvState

instance HasWeshState WeshSession where
  weshStateG = to (weshEnvState . weshSessionEnv)

instance HasLogFunc WeshEnv where
  logFuncL = lens weshEnvLogFunc (\c f -> c {weshEnvLogFunc = f})

instance HasProcessContext WeshSession where
  processContextL = lens weshSessionProcessContext (\c f -> c {weshSessionProcessContext = f})

instance HasLogFunc WeshSession where
  logFuncL =
    lens
    (\c -> weshSessionEnv c ^. logFuncL)
    (\c lf -> c {weshSessionEnv = set logFuncL lf (weshSessionEnv c)})
