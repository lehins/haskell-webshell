{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Database.Persist.Sqlite
import qualified Network.Wai.Handler.Warp as Warp
import RIO
import RIO.Orphans ()
import RIO.Text as T
import Wesh.App
import Yesod (toWaiApp)
import Yesod.Static


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
  appSqlBackendPool <- runSimpleApp $ createSqlitePool (T.pack (sqliteFile opts)) 1
  appStatic <- staticDevel "files/static"
  appWeshState <- newIORef ()
  logOptions <- logOptionsHandle stdout True
  withLogFunc logOptions $ \ appLogFunc -> do
    app <- Yesod.toWaiApp $ App {appSqlBackendPool, appStatic, appWeshState, appLogFunc}
    Warp.runSettings warpSettings app
