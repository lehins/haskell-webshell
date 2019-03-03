{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Wesh.Connect where

import Conduit
import Control.Monad.Reader (withReaderT)
import Network.WebSockets
import RIO
import RIO.Process
import Yesod (MonadHandler)
import qualified Yesod.WebSockets as WS

import Wesh.Parser
import Wesh.Terminal
import Wesh.Types

attemptCommunication :: (MonadHandler m, MonadUnliftIO m) => Text -> WeshEnv -> m ()
attemptCommunication token weshEnv = do
  weshEnvProcessContext <- mkDefaultProcessContext
  WS.webSockets $
    withReaderT
      (WeshSession weshEnv weshEnvProcessContext)
      (liftRIO (communicate token))

-- | Generalize the environment from @ReaderT Connection@ to @RIO env@
fromWebSocketsT :: HasConnection env => WS.WebSocketsT IO a -> RIO env a
fromWebSocketsT ws = RIO (withReaderT (^. connectionG) ws)

sourceWSText :: HasConnection env => ConduitT i ByteString (RIO env) ()
sourceWSText = transPipe fromWebSocketsT WS.sourceWS

sinkWSText :: ConduitT ByteString o (RIO WeshSession) ()
sinkWSText = transPipe fromWebSocketsT WS.sinkWSText

communicate :: Text -> RIO WeshSession ()
communicate token = do
  eExitCode <-
    tryAny $
    withTerminal token cmd args $ \Terminal {tInputSink, tOutputSource} ->
      let terminalOutput = tOutputSource .| debugConduit "Output" .| sinkWSText
          terminalInput = sourceWSText .| debugConduit "Input" .| tInputSink
          runCommunication =
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
    cmd = "/usr/bin/docker"
    args = ["run", "--rm", "-ith", "wesh", "lehins/lehins", "bash"]
    -- cmd = "/bin/bash"
    -- args = []


debugConduit ::
     (MonadIO m, MonadReader env m, HasLogFunc env)
  => Utf8Builder
  -> ConduitT ByteString ByteString m ()
debugConduit channel = iterMC logChars
  where
    logChars bs =
      case tParse bs of
        Left err ->
          logWarn $ "Couldn't parse the control sequence: " <> fromString err
        Right ctlSeq -> logDebug $ channel <> ": " <> displayShow ctlSeq
