{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Database.Persist.Sqlite (createSqlitePool)
import qualified Network.Wai.Handler.Warp as Warp
import Options.Applicative
import RIO
import RIO.Orphans ()
import RIO.Text as T
import Yesod (toWaiApp)
import Yesod.Static
import RIO.Directory
import RIO.FilePath

import Wesh.App
import Wesh.Types

data Options = Options
  { port       :: Int
  , host       :: String
  , logLevel   :: LogLevel
  , verbose    :: Bool
  , sqliteFile :: FilePath
  }

readLogLevel :: ReadM LogLevel
readLogLevel =
  maybeReader $ \case
    "debug" -> Just LevelDebug
    "info"  -> Just LevelInfo
    "warn"  -> Just LevelWarn
    "error" -> Just LevelError
    _       -> Nothing


optionsParser :: FilePath -> Parser Options
optionsParser defDataDir =
  Options <$>
  option
    auto
    (long "port" <> short 'p' <> value defPort <>
     help ("Port number for the webserver (default: " <> show defPort <> ")")) <*>
  strOption
    (long "host" <> short 'h' <> value defHost <>
     help ("Host name for the webserver (default: '" <> defHost <> "')")) <*>
  option
    readLogLevel
    (long "log-level" <> short 'l' <> value LevelWarn <>
     help
       "Minimum log level (debug|info|warn|error) to be printing to stdout (default: 'warn')") <*>
  switch
    (long "verbose" <> short 'v' <>
     help "Make the server verbose (Off by default)") <*>
  strOption
    (long "sqlite" <> value defSqlite <>
     help
       ("Database file for the SQLite backend (default: '" <> defSqlite <> "')"))
  where
    defPort = 3000
    defHost = "localhost"
    defSqlite = defDataDir </> "db.sqlite"

main :: IO ()
main = do
  defDataPath <- getXdgDirectory XdgData "wesh"
  opts <-
    execParser $
    info
      (optionsParser defDataPath <*
       abortOption
         ShowHelpText
         (long "help" <> short 'h' <> help "Display this message."))
      (header "wesh - WebShell service " <>
       progDesc
         "A webserver that that spawns local terminals \
         \and pipes communication over websockets to and form the user browser")
  let warpSettings =
        Warp.setPort (port opts) $
        Warp.setHost (fromString (host opts)) Warp.defaultSettings
  createDirectoryIfMissing True (takeDirectory (sqliteFile opts))
  appSqlBackendPool <-
    runRIO (mempty :: LogFunc) $ createSqlitePool (T.pack (sqliteFile opts)) 1
  appStatic <- staticDevel "files/static"
  defLogOptions <- logOptionsHandle stdout (verbose opts)
  let logOptions = setLogMinLevel (logLevel opts) defLogOptions
  withLogFunc logOptions $ \weshEnvLogFunc -> do
    weshEnvState <- newIORef mempty
    let appWeshEnv = WeshEnv {weshEnvState, weshEnvLogFunc}
    app <- Yesod.toWaiApp $ App {appSqlBackendPool, appStatic, appWeshEnv}
    Warp.runSettings warpSettings app
