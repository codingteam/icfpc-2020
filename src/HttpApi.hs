{-# LANGUAGE OverloadedStrings #-}

module HttpApi
     ( getResponseFromAliens
     , sendMessageToAliens
     , submission

     -- * Helpers
     , httpLogRun
     ) where

import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.List (intercalate)
import Data.String (fromString)
import Data.Maybe (fromMaybe)
import Data.Functor ((<&>))

import Control.Exception (SomeException, catch)

import Network.HTTP.Simple

import Newtypes
import Modulator
import Helpers


getResponseFromAliens :: BaseUrl -> AliensResponseId -> IO Request
getResponseFromAliens baseUrl responseId =
  parseRequest $
    "GET " <> fromBaseUrl baseUrl <> "/aliens/" <>
    fromAliensResponseId responseId


sendMessageToAliens :: BaseUrl -> CallToAliens -> IO Request
sendMessageToAliens baseUrl callToAliens =
  parseRequest ("POST " <> fromBaseUrl baseUrl <> "/aliens/send")
    <&> setRequestBodyLBS (printBits $ modulate callToAliens)


submission :: Maybe ApiKey -> Request -> IO ()
submission apiKey request =
  commitRequest `catch` exceptionHandler

  where
    exceptionHandler :: SomeException -> IO ()
    exceptionHandler = fail . ("Unexpected server response:\n" <>) . show

    patchRequest =
      fromMaybe id $
        apiKey <&> fromApiKey <&> fromString <&> \x ->
          addToRequestQueryString [("apiKey", Just x)]

    commitRequest = do
      let finalRequest = patchRequest request
      errPutStrLn ("Committing request: " <> show finalRequest)
      response <- httpLBS finalRequest

      case getResponseStatusCode response of
           200 ->
             putStrLn $
               "Server response: " <> BLU.toString (getResponseBody response)

           statusCode ->
             fail $ mconcat
                  [ "Unexpected server response:\n"
                  , "  HTTP code: " <> show statusCode <> "\n"
                  , "  Response body: " <>
                         BLU.toString (getResponseBody response)
                  ]


-- * Helpers

httpLogRun :: BaseUrl -> Maybe PlayerKey -> Maybe ApiKey -> IO ()
httpLogRun baseUrl playerKey apiKey
  = errPutStrLn
  $ intercalate "\n"
  [ "ServerUrl: " <> show baseUrl
  , "PlayerKey: " <> maybe "(not set)" show playerKey
  , "ApiKey: "    <> maybe "(not set)" show apiKey
  ]
