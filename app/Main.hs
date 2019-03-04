{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Database.Persist.Sqlite (createSqlitePool)
import qualified Network.Wai.Handler.Warp as Warp
import RIO
import RIO.Orphans ()
import RIO.Text as T
import Yesod (toWaiApp)
import Yesod.Static

import Wesh.App
import Wesh.Types

data Options = Options
  { port       :: Int
  , host       :: String
  , sqliteFile :: FilePath
  }


main :: IO ()
main = do
  let opts =
        Options
          { port = 3000
          , host = "localhost"
          , sqliteFile = "/home/lehins/tmp/users.sqlite"
          }
      warpSettings =
        Warp.setPort (port opts) $
        Warp.setHost (fromString (host opts)) Warp.defaultSettings
  appSqlBackendPool <-
    runRIO (mempty :: LogFunc) $ createSqlitePool (T.pack (sqliteFile opts)) 1
  appStatic <- staticDevel "files/static"
  logOptions <- logOptionsHandle stdout True
  withLogFunc logOptions $ \weshEnvLogFunc -> do
    weshEnvState <- newIORef mempty
    let appWeshEnv = WeshEnv {weshEnvState, weshEnvLogFunc}
    app <- Yesod.toWaiApp $ App {appSqlBackendPool, appStatic, appWeshEnv}
    Warp.runSettings warpSettings app
