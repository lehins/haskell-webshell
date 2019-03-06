{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Wesh.Connect where

import Conduit
import Control.Monad.Reader (withReaderT)
import Network.WebSockets (ConnectionException(CloseRequest))
import RIO
import RIO.Process
import Yesod (MonadHandler)
import qualified Yesod.WebSockets as WS

import Wesh.Parser
import Wesh.Terminal
import Wesh.Types

attemptCommunication :: (MonadHandler m, MonadUnliftIO m) => Token -> WeshEnv -> m ()
attemptCommunication token weshEnv = do
  weshEnvProcessContext <- mkDefaultProcessContext
  let session = WeshSession weshEnv weshEnvProcessContext
  WS.webSockets $ withReaderT session (liftRIO (communicate token))

-- | Generalize the environment from @ReaderT Connection@ to @RIO env@
fromWebSocketsT :: HasConnection env => WS.WebSocketsT IO a -> RIO env a
fromWebSocketsT ws = RIO (withReaderT (^. connectionG) ws)

sourceWSText :: HasConnection env => ConduitT i ByteString (RIO env) ()
sourceWSText = transPipe fromWebSocketsT WS.sourceWS

sinkWSText :: ConduitT ByteString o (RIO WeshSession) ()
sinkWSText = transPipe fromWebSocketsT WS.sinkWSText

communicate :: Token -> RIO WeshSession ()
communicate token = do
  eExitCode <-
    tryAny $
    withShell token cmd args $ \ t ->
      let terminalOutput = terminalOutputSource t .| debugConduit "Output" .| sinkWSText
          terminalInput = sourceWSText .| debugConduit "Input" .| terminalInputSink t
          runCommunication = do
            runConduit (yield "\ESC[?25h" .| sinkWSText) -- turn on cursor
            race_ (runConduit terminalOutput) (runConduit terminalInput)
       in catch runCommunication $ \case
            exc@CloseRequest {} ->
              logInfo $ "Session was closed by the client: " <> displayShow exc
            exc -> logError $ "Session was terminated: " <> displayShow exc
  case eExitCode of
    Left exc ->
      logError $ "Error running a terminal connection: " <> displayShow exc
    Right (Right exitCode) -> logDebug $ "Exited with: " <> displayShow exitCode
    Right _ -> pure ()
  where
    cmd = "/bin/bash"
    args = []


debugConduit ::
     (MonadIO m, MonadReader env m, HasLogFunc env)
  => Utf8Builder -> ConduitT ByteString ByteString m ()
debugConduit channel = iterMC logChars
  where logChars bs =
          case tParse bs of
            Left err ->
              logWarn $ "Couldn't parse the control sequence: " <> fromString err
            Right ctlSeq -> logDebug $ channel <> ": " <> displayShow ctlSeq
