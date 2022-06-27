{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib2 where

import Control.Concurrent
import Control.Concurrent.STM.TVar
  ( TVar,
    newTVar,
    readTVar,
    writeTVar,
  )
import Control.Exception hiding (Handler)
import Control.Monad.IO.Class
import Control.Monad.STM
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Text.IO hiding (putStrLn)
import Debug.Trace (putTraceMsg)
import GHC.Generics
import Network.HTTP.Client hiding (Proxy)
import Network.Wai.Handler.Warp
import Servant
import Servant.Client

newtype Book = Book String deriving (Show, Generic)

instance ToJSON Book

instance FromJSON Book

type GetBooks = Get '[JSON] [Book]

type AddBook = ReqBody '[JSON] Book :> PostCreated '[JSON] Book

type BooksAPI = "books" :> (GetBooks :<|> AddBook)

api :: Proxy BooksAPI
api = Proxy

newtype State = State {books :: TVar [Book]}

type AppM = ReaderT State Handler

server :: ServerT BooksAPI AppM
server = getBooks :<|> addBook
  where
    getBooks = do
      State {books = p} <- ask
      liftIO $ atomically $ readTVar p

    addBook :: Book -> AppM Book
    addBook book = do
      State {books = p} <- ask
      liftIO $ atomically $ readTVar p >>= writeTVar p . (book :)
      return book

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

app :: State -> Application
app s = serve api $ hoistServer api (nt s) server

startApp =
  do
    let port = 8080
    mgr <- newManager defaultManagerSettings
    initialBooks <- atomically $ newTVar []
    let runApp = run port $ app $ State initialBooks
    bracket (forkIO runApp) killThread $ \_ -> do
      let getBooksClient :<|> addBookClient = client api
      let printBooks = getBooksClient >>= liftIO . print
      _ <-
        flip runClientM (mkClientEnv mgr (BaseUrl Http "localhost" port "")) $
          do
            _ <- printBooks
            _ <- addBookClient $ Book "Harry Potter and the Order of the Phoenix"
            _ <- printBooks
            _ <- addBookClient $ Book "To Kill a Mockingbird"
            _ <- printBooks
            _ <- addBookClient $ Book "The Picture of Dorian Gray"
            printBooks
      return ()
