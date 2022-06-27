{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | https://docs.servant.dev/en/stable/cookbook/using-free-client/UsingFreeClient.html
module Lib3 (startApp) where

import Control.Monad.Free
import qualified Network.HTTP.Client as HTTP
import Network.Wai.Handler.Warp
import Servant
import Servant.Client.Free
import qualified Servant.Client.Internal.HttpClient as I
import System.Environment

type API = "square" :> Capture "n" Int :> Get '[JSON] Int

api :: Proxy API
api = Proxy

getSquare :: Int -> Free ClientF Int
getSquare = client api

test :: IO ()
test = case getSquare 42 of
  Pure n ->
    putStrLn $ "Error : got pure result: " ++ show n
  Free (Throw err) ->
    putStrLn $ "Error : got error right away" ++ show err
  Free (RunRequest req k) -> do
    burl <- parseBaseUrl "http://localhost:8000"
    mgr <- HTTP.newManager HTTP.defaultManagerSettings
    let req' = I.defaultMakeClientRequest burl req
    putStrLn $ "Making request" ++ show req'
    res' <- HTTP.httpLbs req' mgr
    putStrLn $ "Got response : " ++ show res'

startApp :: IO ()
startApp = do
  args <- getArgs
  case args of
    ("server" : _) -> do
      putStrLn "Starting cookbook using free client localhost:8000"
      run 8000 $ serve api $ \n -> return (n * n)
    ("client" : _) -> test
    _ -> do
      putStrLn "Try : "
      putStrLn "cabal new run server"
      putStrLn "cabal new run client"
