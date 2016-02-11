{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where
import System.Process
import System.Posix.Unistd (sleep)
--import System.Posix.IO
--import System.Posix.Terminal
--import System.Posix.Process
import Control.Monad.IO.Class (liftIO)


import Happstack.Server     
import Control.Monad ( msum )
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO --(hGetChar, hPutStr, hGetContents, hWaitForInput, Handle)
import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Lazy as LT (Text, unpack, singleton)
import Data.ByteString (ByteString)
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data KashState = KashState { hIn :: MVar Handle, 
                             hOut :: MVar Handle, 
                             hErr :: MVar Handle,
                             hProcessHandle :: ProcessHandle}
                 --consumed :: MVar LT.Text,
                 --unconsumed :: MVar LT.Text }

 

main :: IO ()
main = do
  --(hin, hout, herr, ph) <- runInteractiveProcess "dash" [] Nothing Nothing
  (Just hin, Just hout, Just herr, ph) <- 
    createProcess (proc "bash" ["-c", "\"login\""]) {
       std_in = CreatePipe,
       std_out = CreatePipe, 
       std_err = CreatePipe }
  liftIO $ hSetBuffering hin NoBuffering
  mvhin <- newMVar hin
  mvhout <- newMVar hout
  mvherr <- newMVar herr
  simpleHTTP nullConf (kash (KashState mvhin mvhout mvherr ph))

kashPolicy :: BodyPolicy
kashPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

kash :: KashState -> ServerPart Response
kash st = do 
  decodeBody kashPolicy 
  msum [ dir "login"   $ login
       , dir "echo"    $ echo
       , dir "query"   $ queryParams
       , dir "form"    $ formPage
       , dir "communicate"  $ communicate st
       , dir "communicateBack"  $ communicateBack 15 st
       , dir "fortune" $ fortune
       , dir "static" $ static
       , homePage]
       
static :: ServerPart Response
static = serveDirectory DisableBrowsing ["kasj.js", "jquery.js"] "static"

template :: Text -> Html -> Response
template title body = 
  toResponse $ H.html $ do
    H.head $ do
      H.title (toHtml title)
    H.body $ do
        body
        p $ a ! href "/" $ "back home"

mvhGetContents :: MVar Handle -> IO (Maybe String)
mvhGetContents mvh = do
  let hGetContentsNoClose h s = do 
        ready <- hReady h
        if ready then do
          c <- hGetChar h
          hGetContentsNoClose h $ s++[c]
          else return s
  handle <- takeMVar mvh
  isReady <- hReady handle
  if isReady then do
    content <- hGetContentsNoClose handle ""
    putStrLn ("received out: " ++ content)
    putMVar mvh handle
    return $ Just content
    else do
    putMVar mvh handle
    return $ Nothing

communicateBack :: Int -> KashState -> ServerPart Response
communicateBack att st@(KashState _ mvhout mvherr _) = do
  content <- liftIO $ mvhGetContents mvhout
  case content of 
    (Just c) -> return $ toResponse c
    Nothing -> do
      content <- liftIO $ mvhGetContents mvherr
      case content of 
        (Just c) -> return $ toResponse c
        Nothing -> do
          if att <= 0 then do
            return $ (toResponse :: String -> Response) "" 
            else do
            liftIO $ sleep 1
            communicateBack (att-1) st
  

communicate :: KashState -> ServerPart Response
communicate st@(KashState mvhin _ _ _) = msum [getComm, postComm] where
  commForm :: H.AttributeValue -> Response
  commForm p = 
    template "form" $
    form ! action "/communicate" ! enctype "multipart/form-data" ! A.method "POST" $ do
      label ! A.for "posValt" $ "Post: "
      input ! type_ "text" ! A.id "postVal" ! name "postVal"
      H.br
      label ! A.for "posted" $ "Posted: "
      input ! type_ "text" ! A.id "posted" ! name "posted" ! A.value p
      H.br
      input ! A.type_ "submit" ! A.value "Submit"
                      
  getComm :: ServerPart Response
  getComm = do method GET
               serveFile (asContentType "text/html") "comm.html"
               --ok $ commForm ""
  postComm :: ServerPart Response
  postComm = do method POST
                posted <- lookText "postVal"
                hin <- liftIO $ takeMVar mvhin
                liftIO $ hPutStrLn hin (LT.unpack posted) >> hFlush hin
                liftIO $ putMVar mvhin hin
                liftIO $ putStrLn ("posted: "++(LT.unpack posted))
                return $ toResponse posted


testLs :: ServerPart String
testLs = do
  (_, Just hout, _, _) <-
       liftIO $ createProcess (proc "ls" []){ std_out = CreatePipe }
  cont <- liftIO $ hGetContents hout
  return cont
  

homePage :: ServerPart Response
homePage =
    ok $ template "home page" $ do
      H.h1 "Hello!"
      H.p "Writing applications with happstack-lite is fast and simple!"
      H.p "Check out these killer apps."
      H.p $ a ! href "/login"          $ "Login"
      H.p $ a ! href "/echo/secret%20message"  $ "echo"
      H.p $ a ! href "/query?foo=bar" $ "query parameters"
      H.p $ a ! href "/form"          $ "form processing"
      H.p $ a ! href "/fortune"       $ "(fortune) cookies"
      H.p $ a ! href "/upload"        $ "file uploads"
      
echo :: ServerPart Response
echo =
  path $ \(msg :: String) ->  ok $ template "echo" $ do
    p $ "echo says: " >> toHtml msg
    p "Change the url to echo something else."
    
queryParams :: ServerPart Response
queryParams =
  do mFoo <- optional $ lookText "foo"
     ok $ template "query params" $ do
       p $ "foo is set to: " >> toHtml (show mFoo)
       p $ "change the url to set it to something else."

formPage :: ServerPart Response
formPage = msum [ viewForm, processForm ]
   where
     viewForm :: ServerPart Response
     viewForm =
         do method GET
            ok $ template "form" $
               form ! action "/form" ! enctype "multipart/form-data" ! A.method "POST" $ do
                 label ! A.for "msg" $ "Say something clever"
                 input ! type_ "text" ! A.id "msg" ! name "msg"
                 input ! type_ "submit" ! value "Say it!"

     processForm :: ServerPart Response
     processForm =
         do method POST
            msg <- lookText "msg"
            ok $ template "form" $ do
              H.p "You said:"
              H.p (toHtml msg)


 

login :: ServerPart Response
login = msum [viewLogin, processLogin] where
  loginForm :: H.AttributeValue -> H.AttributeValue -> Response
  loginForm val c = 
    template "form" $
    form ! action "/login" ! enctype "multipart/form-data" ! A.method "POST" $ do
      label ! A.for "username" $ "Username: "
      input ! type_ "text" ! A.class_ c ! A.id "username" ! name "username" ! A.placeholder "username" ! A.value val
      H.br
      label ! A.for "password" $ "Password: "
      input ! type_ "password" ! A.class_ c ! A.id "password" ! name "password" ! A.placeholder "password"
      H.br
      input ! A.type_ "submit" ! A.value "Login"
                      
  viewLogin :: ServerPart Response
  viewLogin = do method GET
                 testls <- testLs

                 ok $ loginForm (H.toValue testls) ""
  processLogin :: ServerPart Response
  processLogin = do method POST
                    username <- lookText "username"
                    password <- lookText "password"
                    let result = 0 --performLogin (unpack username) (unpack password)
                    if result == 0 then
                      --ok "Success" else
                      ok $ loginForm (H.toValue username) "error" else       
                      ok $ loginForm (H.toValue username) "error"
                                            
                                            
                 

fortune :: ServerPart Response
fortune = msum [ viewFortune, updateFortune ]
     where
       viewFortune :: ServerPart Response
       viewFortune =
           do method GET
              mMemory <- optional $ lookCookieValue "fortune"
              let memory = fromMaybe "Your future will be filled with web programming." mMemory
              ok $ template "fortune" $ do
                     H.p "The message in your (fortune) cookie says:"
                     H.p (toHtml memory)
                     form ! action "/fortune" ! enctype "multipart/form-data" ! A.method "POST" $ do
                     label ! A.for "fortune" $ "Change your fortune: "
                     input ! type_ "text" ! A.id "fortune" ! name "new_fortune"
                     input ! type_ "submit" ! value "Say it!"

       updateFortune :: ServerPart Response
       updateFortune =
           do method POST
              fortune <- lookText "new_fortune"
              addCookies [(Session, mkCookie "fortune" (LT.unpack fortune))]
              seeOther ("/fortune" :: String) (toResponse ())