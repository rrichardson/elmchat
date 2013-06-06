{-# LANGUAGE OverloadedStrings, TypeFamilies, DeriveDataTypeable, TemplateHaskell, BangPatterns #-}

module Main where

import Web.Scotty
import Language.Elm (toHtml)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Blaze.ByteString.Builder (Builder)
import Data.Default (def)
import Data.Typeable
import qualified Data.Map as Map
import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- Network
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import Network.Wai (Response, Response(ResponseBuilder, ResponseFile, ResponseSource))
import Network.Wai.Handler.Warp (settingsPort, settingsIntercept )
import Network.HTTP.Types  (status404, status200)
import qualified Network.WebSockets             as WS
import qualified Network.Wai.Handler.WebSockets as WS

-- Database
import Data.Acid
import Data.SafeCopy
import qualified Data.Map as M
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)

import qualified Control.Monad.State as S
import Control.Lens (makeLenses)
import Control.Monad (forever, when)
import Control.Concurrent (forkIO, threadDelay)
import Data.Monoid (mappend)
import Control.Exception (fromException)

import Data.Time.Clock (getCurrentTime)
data Clock = Clock
------------------------------------------------------------------------------

type Key = String
type Value = String

data Database = Database !(Map.Map Key Value)
    deriving (Show, Ord, Eq, Typeable)

$(deriveSafeCopy 0 'base ''Database)
makeLenses ''Database

------------------------------------------------------

insertKey :: Key -> Value -> Update Database ()
insertKey key value
    = do Database m <- S.get
         S.put (Database (Map.insert key value m))

lookupKey :: Key -> Query Database (Maybe Value)
lookupKey key
    = do Database m <- ask
         return (Map.lookup key m)

deleteKey :: Key -> Update Database ()
deleteKey key
    = do Database m <- S.get
         S.put (Database (Map.delete key m))

allKeys :: Int -> Query Database [(Key, Value)]
allKeys limit
    = do Database m <- ask
         return $ take limit (M.toList m)

$(makeAcidic ''Database ['insertKey, 'lookupKey, 'allKeys, 'deleteKey])

------------------------------------------------------------------------------

config db = def { verbose = 0
           , settings = (settings def) { settingsPort = 4000, settingsIntercept = WS.intercept (socketsmain db) }
           }

fixtures :: M.Map String String
fixtures = M.fromList [("one", "1"), ("two", "2"), ("three", "3")]


------------------------------------------------------------------------------
-- MAIN MAIN

main :: IO ()
main = do
    putStrLn "Loading Database."
    db <- openLocalStateFrom "db/" (Database fixtures)

    putStrLn "Starting HTTP Server."
    source <- readFile "Client.elm"
    let index = toHtml "js/elm-runtime.js" "Chat Time!" source 
    scottyOpts (config db) (httpmain index db)

------------------------------------------------------------------------------
-- HTTP MAIN

httpmain :: Html -> AcidState (EventState AllKeys) -> ScottyM ()
httpmain index database = do
        middleware logStdoutDev
        middleware $ staticPolicy (noDots >-> addBase "static")

        get "/" $ blaze index

        get "/read/" $ do
            result <- liftIO $ query database (AllKeys 10)
            json result

        get "/read/:key" $ do
            key <- param "key"
            result <- liftIO $ query database (LookupKey key)
            case result of
               Nothing    -> status status404
               Just value -> json value

        delete "/delete/:key" $ do
            key <- param "key"
            _   <- liftIO $ update database (DeleteKey key)
            status status200

        post "/update/" $ do
            (key, val) <- jsonData
            _          <- liftIO $ update database (InsertKey key val)
            status status200

-- from https://github.com/jb55/scotty-blaze/blob/master/src/Web/Scotty/Blaze.hs
blaze :: Html -> ActionM ()
blaze h = do
  header "Content-Type" "text/html"
  builder $ renderHtmlBuilder h

builder :: Builder -> ActionM ()
builder = S.modify . setContent

setContent :: Builder -> Response -> Response
setContent b (ResponseBuilder s h _) = ResponseBuilder s h b
setContent b (ResponseFile s h _ _)  = ResponseBuilder s h b
setContent b (ResponseSource s h _)  = ResponseBuilder s h b
------------------------------------------------------------------------------
-- Websockets MAIN

type Client = (T.Text, WS.Sink WS.Hybi10)

socketsmain :: AcidState (EventState AllKeys) -> WS.Request -> WS.WebSockets WS.Hybi10 ()
socketsmain database rq = do
    WS.acceptRequest rq
    WS.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
    sink <- WS.getSink
    (WS.receiveData :: WS.WebSockets WS.Hybi10 T.Text) >>= liftIO . T.putStrLn
    talk database ("visitor", sink)

talk :: WS.Protocol p => AcidState (EventState AllKeys) -> Client -> WS.WebSockets p ()
talk db client@(user, _) = flip WS.catchWsError catchDisconnect $
  forever $ do
    !msg <- WS.receiveData
    liftIO $ T.putStrLn msg
    where
      catchDisconnect e = case fromException e of
         Just WS.ConnectionClosed -> liftIO $ T.putStrLn $ user `mappend` (T.pack " disconnected")
         _ -> return ()
