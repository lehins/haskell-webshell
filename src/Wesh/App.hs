{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Wesh.App where

import Data.Aeson as A
import Crypto.Random
import Data.ByteString.Builder
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend, runSqlPool)
import RIO hiding (Handler)
import RIO.Text as T
import Wesh.Connect
import Wesh.Types
import Yesod
import Yesod.Static
import Network.HTTP.Types.Status (badRequest400)

data App = App
  { appSqlBackendPool :: !(Pool SqlBackend)
  , appStatic         :: !Static
  , appWeshEnv        :: !WeshEnv
  }

instance Yesod App where
  makeSessionBackend _ = return Nothing
  shouldLogIO _ _ _ = return True

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB db = do
    pool <- appSqlBackendPool <$> getYesod
    runSqlPool db pool

staticFiles "files/static/"

mkYesod "App" [parseRoutes|
/ HomeR GET
/static StaticR Static appStatic
/terminal/#Text TerminalR GET
/resize/#Text ResizeTerminalR POST
|]

getHomeR :: HandlerFor App Html
getHomeR = do
  randomBS <- liftIO $ getRandomBytes 32
  let token = textDisplay $ Utf8Builder $ byteStringHex randomBS
  -- TODO: Map.lookup token (weshEnvState appWeshEnv)
  defaultLayout $ do
    setTitle "Haskell Web Shell - hwesh"
    addStylesheet $ StaticR wesh_css
    addScriptRemote "https://code.jquery.com/jquery-3.3.1.min.js"
    -- `hterm` created by google, built locally and served as a static file. Source:
    -- https://chromium.googlesource.com/apps/libapps/+/master/hterm
    addScript $ StaticR hterm_all_js
    addScript $ StaticR wesh_js
    [whamlet|<div id="terminal" data-token="#{token}">|]

getTerminalR :: Text -> HandlerFor App ()
getTerminalR token = do
  App {appWeshEnv} <- getYesod
  attemptCommunication token appWeshEnv


data WindowSize = WindowSize
  { wsWidth :: !Int
  , wsHeight :: !Int
  } deriving Show

instance FromJSON WindowSize where
  parseJSON =
    withObject "WindowSize" $ \o -> do
      wsWidth <- o .: "width"
      wsHeight <- o .: "height"
      pure WindowSize {wsWidth, wsHeight}

postResizeTerminalR :: Text -> HandlerFor App Value
postResizeTerminalR token = do
  App {appWeshEnv} <- getYesod
  parseJsonBody >>= \case
    A.Error err -> sendStatusJSON badRequest400 $ makeError ("JSON Parse Error: " <> T.pack err)
    A.Success val -> do
      runRIO appWeshEnv $ RIO.logInfo $ displayShow (val :: WindowSize)
      pure $ object ["success" .= True]
  -- attemptCommunication token appWeshEnv


-- postAuthenticateR :: Handler App Value
-- postAuthenticateR = do
--   parseJsonBody >>= \case
--     A.Error err -> sendStatusJSON badRequest400 $ makeError ("JSON Parse Error: " <> T.pack err)
--     A.Success s -> do

makeError :: Text -> Value
makeError txt = object ["error" .= txt]
