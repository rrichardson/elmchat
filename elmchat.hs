{-# LANGUAGE OverloadedStrings, TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}

module Main where

import Web.Scotty

import Data.Default (def)
import Data.Typeable
import qualified Data.Map as Map
import qualified Data.Aeson as J

-- Network
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
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
import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay)

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
    scottyOpts (config db) (httpmain db)


------------------------------------------------------------------------------
-- HTTP MAIN

httpmain :: AcidState (EventState AllKeys) -> ScottyM ()
httpmain database = do
        middleware logStdoutDev
        middleware $ staticPolicy (noDots >-> addBase "static")

        get "/" $ file "index.html"

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

------------------------------------------------------------------------------
-- Websockets MAIN


socketsmain :: AcidState (EventState AllKeys) -> WS.Request -> WS.WebSockets WS.Hybi00 ()
socketsmain database rq = do
     WS.acceptRequest rq
