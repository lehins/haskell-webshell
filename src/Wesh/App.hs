{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Wesh.App where

import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend, runSqlPool)
import RIO hiding (Handler)
import Yesod
import Yesod.Static
import Wesh.Types
import Wesh.Connect

data App = App
  { appSqlBackendPool :: !(Pool SqlBackend)
  , appStatic :: !Static
  , appWeshState :: !WeshState
  , appLogFunc :: !LogFunc
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
|]


getHomeR :: HandlerFor App Html
getHomeR = do
  app <- getYesod
  webSockets (WeshEnv (appWeshState app) (appLogFunc app)) communicate
  defaultLayout $ do
    setTitle "Haskell Web Shell - hwesh"
    addStylesheet $ StaticR wesh_css
    -- addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/jquery.terminal/2.2.0/css/jquery.terminal.min.css"
    -- addScriptRemote "https://unpkg.com/js-polyfills/keyboard.js"
    -- addScriptRemote "https://cdn.terminal.jcubic.pl/wcwidth.js"
    -- addScriptRemote "https://code.jquery.com/jquery-3.3.1.min.js"
    -- addScriptRemote "https://code.jquery.com/ui/1.12.1/jquery-ui.min.js"
    -- addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/jquery.terminal/2.2.0/js/jquery.terminal.min.js"
    --addScriptRemote "https://raw.githubusercontent.com/atdt/escapes.js/master/escapes.js"
    -- addScript $ StaticR escapes_js
    addScript $ StaticR hterm_all_js
    addScript $ StaticR wesh_js
    -- toWidget
    --   [julius|
    --          wesh();
    --   |]
    [whamlet|
            <div id="terminal">
    |]
