{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where
import Kash.Terminal
import Kash.Interpreter
import System.Posix.Unistd (sleep, usleep)
import Control.Monad.IO.Class (liftIO)


import Happstack.Server
import Happstack.Server.Cookie (addCookie)
import Control.Monad ( msum )
import Control.Monad.STM
import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.HashTable
import Data.UUID.V4 (nextRandom)
import Codec.Binary.Url
import qualified Data.UUID as UUID
import qualified Data.Text.Lazy as LT (Text, unpack, singleton, empty, snoc)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Prelude hiding (lookup)

type Terminals = HashTable String Terminal
data KashSession = KashSession Cookie Terminals
type Environment = HashTable String KashSession

sidName = "kash_sid"

main :: IO ()
main = do
  --t <- spawnTerminal "/bin/login" []
  env <- new (==) hashString
  simpleHTTP nullConf $ kash env

kashPolicy :: BodyPolicy
kashPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

getTerminals :: KashSession -> IO Terminals
getTerminals (KashSession _ ts) = return ts

getTerminal :: Terminals -> String -> IO (Maybe Terminal)
getTerminal ts key = lookup ts key

createTerminal :: FilePath -> [String] -> Terminals -> IO Terminal
createTerminal cmd args ts = do
  t <- spawnTerminal cmd args
  tid <- tGetId t
  insert ts (show tid) t
  return t


getSession = lookup
createSession env = do
  uuid <- nextRandom
  let sid = UUID.toString uuid
  -- Create holder for Terminals for that session
  ts <- new (==) hashString
  -- Create new session
  let newSession = KashSession (mkCookie sidName sid) ts
  -- Put Session into the enviroment
  insert env sid newSession
  return newSession

-- ------------------------------------------------------------------------------
-- | Gets the current session. If a session doesn't exist or invalid a new session
-- will be created and a new cookie value will be set.
--
getOrCreateSession :: Environment -> String -> IO KashSession
getOrCreateSession env sid = do
  -- Get session id from request. If failed to find legit sid ms will be Nothing
  ms <- lookup env sid
  -- Get Session or create new one if failed
  case ms of
    (Just s) -> return s
    Nothing -> do
      -- Create a new random session id
      uuid <- nextRandom
      -- Create holder for Terminals for that session
      t <- new (==) hashString
      -- Create new session
      let newSession = KashSession (mkCookie sidName $ UUID.toString uuid) t
      -- Put Session into the enviroment
      insert env (UUID.toString uuid) newSession
      return newSession

kash :: Environment -> ServerPart Response
kash env = do
  let newSession = do
        s@(KashSession c ts) <- liftIO $ createSession env
        addCookie Session c
        return s
  decodeBody kashPolicy
  msid <- optional $ lookCookieValue sidName
  ks@(KashSession c ts) <- case msid of
    (Just sid) -> do
      ms <- liftIO $ getSession env sid
      case ms of
        (Just s) -> return s
        Nothing -> newSession
    Nothing -> newSession
  msum [ dir "terminal"   $ terminalView ts,
         dir "terminalPull" $ terminalPullResponse ts,
         dir "terminalPush" $ terminalPushResponse ts,
         dir "static" $ static,
         homePage]

-- | Serves static files like JavaScript and StyleSheet files.
static :: ServerPart Response
static = serveDirectory EnableBrowsing [] "static"

-- | Main template that includes all necessary static files tags and other base
-- stuff that appears on every page
template :: Text -> Html -> Response
template title body =
  toResponse $ H.docTypeHtml ! A.lang "en" !
      A.xmlns "http://www.w3.org/1999/xhtml" $ do
    H.head $ do
      let cssTag href = H.link ! A.rel "stylesheet" ! A.media "all" !
                        A.type_ "text/css" ! A.href href
      let jsTag src = H.script ! A.type_ "text/javascript" ! A.src src $ ""
      H.title $ toHtml title
      cssTag "static/jquery/jquery-ui.min.css"
      cssTag "static/bootstrap/css/bootstrap.min.css"
      cssTag "static/kash.css"
      jsTag "static/jquery/jquery-1.9.1.min.js"
      jsTag "static/jquery/jquery-ui.min.js"
      jsTag "static/bootstrap/js/bootstrap.min.js"
      jsTag "static/kash.js"
    H.body body

processOutput content = return $ toResponse content

-- could use the below one for current one but, below one will include parsing
emptyResponse :: ServerPart Response
emptyResponse = responseFromOutput ""

responseFromOutput :: B.ByteString -> ServerPart Response
responseFromOutput str = return $ toResponse str

strToResponse :: String -> Response
strToResponse = toResponse

getTerminalOutput :: Int -> Terminal -> IO B.ByteString
getTerminalOutput att t = do
  output <- tGetStr t
  case output of
    (Just c) -> return $ tToHtml c
    Nothing -> do
      if att <= 0 then do
        return ""
        else do
        sleep 1
        getTerminalOutput (att-1) t

terminalPullResponse :: Terminals -> ServerPart Response
terminalPullResponse = terminalApplyResponse (\t _ ->
                                               getTerminalOutput 15 t)

terminalPushResponse :: Terminals -> ServerPart Response
terminalPushResponse ts = do
  posted <- lookText "streamPush"
  terminalApplyResponse (\t _ -> do
                            case decode $ LT.unpack posted of
                              (Just input) -> do
                                tPutWords t input
                                getTerminalOutput 0 t
                              Nothing -> return ""
                            ) ts

terminalApplyResponse :: (Terminal -> String -> IO B.ByteString) ->
                         Terminals -> ServerPart Response
terminalApplyResponse f ts = do
  mtid <- optional $ lookText "tid"
  case mtid of
    (Just tid) -> do
      let stid = LT.unpack tid
      mt <- liftIO $ getTerminal ts stid
      case mt of
        (Just t) -> do
          output <- liftIO $ f t stid
          responseFromOutput output
        Nothing -> notFound $ strToResponse $
                   "Terminal with tid: \""++stid++"\" not found."
    Nothing -> badRequest $ strToResponse "Terminal id is required."

terminalView :: Terminals -> ServerPart Response
terminalView ts = msum [getComm, postComm] where
  getComm :: ServerPart Response
  getComm = do method GET
               serveFile (asContentType "text/html") "comm.html"
  postComm :: ServerPart Response
  postComm = do method POST
                action <- lookText "action"
                case action of
                  "create" -> do
                    t <- liftIO $ createTerminal "/bin/login" [] ts
                    tid <- liftIO $ tGetId t
                    responseFromOutput $ B8.pack $ show $ tid
                  "delete" -> terminalApplyResponse
                              (\t tid -> do
                                  delete ts tid
                                  tKill t
                                  return "success") ts

homePage :: ServerPart Response
homePage =
    ok $ template "Kash Terminal" $ do
      H.h1 "добрый день!"
      H.p "Here is a semi-functional server terminal."
      H.p $ a ! A.class_ "btn btn-large" ! A.target "_blank" ! href "/terminal" $ "Terminal"
