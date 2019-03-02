module Wesh.Types where

import RIO
import Network.WebSockets.Connection

type WeshState = IORef ()

data WeshEnv = WeshEnv
  { weshEnvState :: !WeshState
  , weshLogFunc :: !LogFunc
  , weshEnvConn :: !Connection
  }

class HasConnection env where
  connectionL :: Lens' env Connection

instance HasConnection WeshEnv where
  connectionL = lens weshEnvConn (\c f -> c {weshEnvConn = f})

instance HasLogFunc WeshEnv where
  logFuncL = lens weshLogFunc (\c f -> c {weshLogFunc = f})

