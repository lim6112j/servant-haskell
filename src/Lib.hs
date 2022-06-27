{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Lib where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT (..))
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Aeson as JSON
import Data.Int (Int64 (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.Persist
import Database.Persist.MySQL
  ( ConnectInfo (..),
    SqlBackend (..),
    defaultConnectInfo,
    fromSqlKey,
    runMigration,
    runSqlPool,
    toSqlKey,
    withMySQLConn,
  )
import Database.Persist.Sql (SqlPersistT, runSqlConn)
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import Database.Persist.Types (PersistValue (PersistInt64))
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant (Handler, throwError)
import Servant.API
import System.Environment (getArgs)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Person json
    Id   Int Primary Unique
    name Text
    age  Text
    deriving Eq Show Generic
|]

type Api =
  "person" :> Get '[JSON] [Person]
    :<|> "person" :> Capture "id" Int :> Get '[JSON] Person
    :<|> "person" :> Capture "id" Int :> Delete '[JSON] ()
    :<|> "person" :> ReqBody '[JSON] Person :> Post '[JSON] Person

apiProxy :: Proxy Api
apiProxy = Proxy

app :: Application
app = serve apiProxy server

-- Run a database operation, and lift the result into a Handler.
-- This minimises usage of IO operations in other functions
runDB :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> Handler a
runDB a = liftIO $ runNoLoggingT $ runResourceT $ withMySQLConn connInfo $ runSqlConn a

-- Change these out to suit your local setup
connInfo :: ConnectInfo
connInfo = defaultConnectInfo {connectHost = "127.0.0.1", connectUser = "root", connectPassword = "", connectDatabase = "testdb"}

doMigration :: IO ()
doMigration = runNoLoggingT $ runResourceT $ withMySQLConn connInfo $ runReaderT $ runMigration migrateAll

server :: Server Api
server =
  personGET
    :<|> personGETById
    :<|> personDELETE
    :<|> personPOST
  where
    personGET = selectPersons
    personGETById id = selectPersonById id
    personDELETE id = deletePerson id
    personPOST personJson = createPerson personJson

selectPersons :: Handler [Person]
selectPersons = do
  personList <- runDB $ selectList [] []
  return $ map (\(Entity _ u) -> u) personList

selectPersonById :: Int -> Handler Person
selectPersonById id = do
  sqlResult <- runDB $ get $ PersonKey id
  case sqlResult of
    Just person -> return person
    Nothing -> throwError err404 {errBody = JSON.encode ("Person with id not exists" :: [Char])}

createPerson :: Person -> Handler Person
createPerson person = do
  attemptCreate <- runDB $ insert person
  case attemptCreate of
    PersonKey k -> return person
    _ -> throwError err503 {errBody = JSON.encode ("Could not create Person." :: [Char])}

deletePerson :: Int -> Handler ()
deletePerson id = do runDB $ delete $ PersonKey id

startApp :: IO ()
startApp = do
  args <- getArgs
  let arg1 = if not (null args) then Just (head args) else Nothing
  case arg1 of
    Just "migrate" -> doMigration
    _ -> run 8080 app
