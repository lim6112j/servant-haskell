{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | https://docs.servant.dev/en/stable/cookbook/custom-errors/CustomErrors.html
module Lib4 (startApp) where

import Data.Aeson
import Data.Proxy
import Data.String.Conversions (cs)
import Data.Text
import Database.Persist (UpdateException (KeyNotFound))
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API.ContentTypes
import Servant.Server (ServerError (errBody))
import Servant.Server.Internal.ErrorFormatter (ErrorFormatters (notFoundErrorFormatter))

newtype Greet = Greet {_msg :: Text} deriving (Generic, Show)

instance FromJSON Greet

instance ToJSON Greet

type TestApi =
  "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet
    :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] Greet
    :<|> "greet" :> Capture "greetId" Text :> Delete '[JSON] NoContent

testApi :: Proxy TestApi
testApi = Proxy

server :: Server TestApi
server = helloH :<|> postGreetH :<|> deleteGreetH
  where
    helloH name Nothing = helloH name (Just False)
    helloH name (Just False) = return . Greet $ "hello, " <> name
    helloH name (Just True) = return . Greet . toUpper $ "hello, " <> name
    postGreetH greet = return greet
    deleteGreetH _ = return NoContent

customFormatter :: ErrorFormatter
customFormatter tr req err =
  let value = object ["combinator" .= show tr, "error" .= err]
      accH = getAcceptHeader req
   in case handleAcceptH (Proxy :: Proxy '[JSON]) accH value of
        Nothing -> err400 {errBody = cs err}
        Just (ctypeH, body) ->
          err400
            { errBody = body,
              errHeaders = [("Content-Type", cs ctypeH)]
            }

notFoundFormatter :: NotFoundErrorFormatter
notFoundFormatter req = err404 {errBody = cs $ "Not found path: " <> rawPathInfo req}

customFormatters :: ErrorFormatters
customFormatters =
  defaultErrorFormatters
    { bodyParserErrorFormatter = customFormatter,
      notFoundErrorFormatter = notFoundFormatter
    }

app :: Application
app = serveWithContext testApi (customFormatters :. EmptyContext) server

startApp :: IO ()
startApp = run 8000 app
