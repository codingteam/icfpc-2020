{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE TupleSections, TypeApplications #-}

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
import Text.Read (readEither)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable, typeRep)

import Control.Exception (SomeException, catch)

import Network.HTTP.Simple hiding (Proxy)

import Newtypes
import Modulator
import Helpers


getResponseFromAliens
  :: BaseUrl
  -> AliensResponseId
  -> IO (Request, Proxy String)

getResponseFromAliens baseUrl responseId
  = fmap (, Proxy)
  $ parseRequest
  $ "GET " <> fromBaseUrl baseUrl <> "/aliens/" <>
    fromAliensResponseId responseId


sendMessageToAliens
  :: BaseUrl
  -> CallToAliens
  -> IO (Request, Proxy Bits)

sendMessageToAliens baseUrl callToAliens
  = fmap (, Proxy)
  $ parseRequest ("POST " <> fromBaseUrl baseUrl <> "/aliens/send")
      <&> setRequestBodyLBS (fromString $ show $ modulate callToAliens)


submission
  :: forall response
   . (SubmissionResponse response, Show response, Typeable response)
  => Maybe ApiKey
  -> (Request, Proxy response)
  -> IO response

submission apiKey (request, Proxy) = go where
  go =
    commitRequest `catch` \(e :: SomeException) ->
      fail ("Unexpected server response:\n" <> show e)

  patchRequest =
    fromMaybe id $
      apiKey <&> fromApiKey <&> fromString <&> \x ->
        addToRequestQueryString [("apiKey", Just x)]

  commitRequest = do
    let finalRequest = patchRequest request
    errPutStrLn ("Committing request: " <> show finalRequest)
    response <- httpLBS finalRequest
    let body = BLU.toString (getResponseBody response)
    parsedBody <- liftEither (parseResponse body)

    case getResponseStatusCode response of
         200 ->
           (parsedBody <$) $ errPutStrLn $
             intercalate "\n"
               [ "Server response:"
               , "  Response type: " <> show (typeRep (Proxy @response))
               , "  Show'n response value: " <> show parsedBody
               ]

         statusCode ->
           fail $ intercalate "\n"
                [ "Unexpected server response:"
                , "  HTTP code: " <> show statusCode
                , "  Response body: " <> show body
                ]


class SubmissionResponse returnType where
  parseResponse :: String -> Either String returnType

instance SubmissionResponse String where
  parseResponse = pure

instance SubmissionResponse Bits where
  parseResponse = readEither


-- * Helpers

httpLogRun :: BaseUrl -> Maybe PlayerKey -> Maybe ApiKey -> IO ()
httpLogRun baseUrl playerKey apiKey
  = errPutStrLn
  $ intercalate "\n"
  [ "ServerUrl: " <> show baseUrl
  , "PlayerKey: " <> maybe "(not set)" show playerKey
  , "ApiKey: "    <> maybe "(not set)" show apiKey
  ]
