{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Wesh.Connect where

import Conduit
import Control.Monad.Reader
import Network.WebSockets.Connection
import RIO
import RIO.ByteString as B
import Yesod (MonadHandler)
import qualified Yesod.WebSockets as WS

import Wesh.Parser
import Wesh.Terminal
import Wesh.Types

webSockets ::
     (MonadHandler m, MonadUnliftIO m)
  => (Connection -> env)
  -- ^ Take a function that captures the connection into our environment
  -> RIO env ()
  -- ^ Action to run for the case of a websocket connection
  -> m ()
webSockets mkEnv rio = WS.webSockets $ withReaderT mkEnv (liftRIO rio)

-- | Generalize the environment from @ReaderT Connection@ to @RIO env@
fromWebSocketsT :: HasConnection env => WS.WebSocketsT IO a -> RIO env a
fromWebSocketsT ws = RIO (withReaderT (^. connectionL) ws)

sourceWSText :: HasConnection env => ConduitT i ByteString (RIO env) ()
sourceWSText = transPipe fromWebSocketsT WS.sourceWS

sinkWSText :: ConduitT ByteString o (RIO WeshEnv) ()
sinkWSText = transPipe fromWebSocketsT WS.sinkWSText

communicate :: RIO WeshEnv ()
communicate = do
  t <- liftIO $ createTerminal "/bin/bash" []
  let terminalOutput = terminalSource t .| sinkWSText
      terminalInput = sourceWSText .| debugConduit "Input" .| terminalSink t
  race_ (runConduit terminalOutput) (runConduit terminalInput)


debugConduit ::
     (MonadIO m, MonadReader env m, HasLogFunc env)
  => Utf8Builder
  -> ConduitT ByteString ByteString m ()
debugConduit channel = iterMC (logDebug . (channel <>) . (": " <>) . displayBytesUtf8)




tToHtml :: ByteString -> ByteString
tToHtml s = B.concat [fil x | x <- parsed]
  where
    parsed =
      case tParse s of
        (Left _)  -> []
        (Right p) -> p
    fil (KString str) = str
    fil _             = B.empty


