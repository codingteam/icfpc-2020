{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators #-}
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
import Data.Typeable (Typeable)

import Control.Exception (SomeException, catch)

import Network.HTTP.Simple hiding (Proxy)

import Demodulator
import Helpers
import Invaluator (Data)
import Modulator
import Newtypes
import Bits (Bits)


type ReqBodyForLog = Maybe BLU.ByteString

getResponseFromAliens
  :: BaseUrl
  -> AliensResponseId
  -> IO (Request, ReqBodyForLog, Proxy String)

getResponseFromAliens baseUrl responseId
  = fmap (, Nothing, Proxy)
  $ parseRequest
  $ "GET " <> fromBaseUrl baseUrl <> "/aliens/" <>
    fromAliensResponseId responseId


sendMessageToAliens
  :: (Modulatable msg, response `TypeIsOneOf` '[Bits, Data])
  => BaseUrl
  -> msg
  -> IO (Request, ReqBodyForLog, Proxy response)

sendMessageToAliens baseUrl msg
  = fmap (, Just reqBody, Proxy)
  $ parseRequest ("POST " <> fromBaseUrl baseUrl <> "/aliens/send")
      <&> setRequestBodyLBS reqBody
  where
    reqBody = fromString $ show $ modulate msg


submission
  :: forall response
   . (SubmissionResponse response, Show response, Typeable response)
  => Maybe ApiKey
  -> (Request, ReqBodyForLog, Proxy response)
  -> IO response

submission apiKey (request, reqBodyForLog, Proxy) = go where
  go =
    commitRequest `catch` \(e :: SomeException) ->
      fail ("Unexpected server response:\n" <> show e)

  patchRequest =
    fromMaybe id $
      apiKey <&> fromApiKey <&> fromString <&> \x ->
        addToRequestQueryString [("apiKey", Just x)]

  commitRequest = do
    let finalRequest = patchRequest request

    errPutStrLn $ intercalate "\n"
      [ -- "Committing request: " <> show finalRequest
       "Request body: " <> show reqBodyForLog
      ]

    response <- httpLBS finalRequest
    let body = BLU.toString (getResponseBody response)
    parsedBody <- liftEither (parseResponse body)

    case getResponseStatusCode response of
         200 ->
           (parsedBody <$) $ errPutStrLn $
             intercalate "\n"
               [ "Server response:"
               , "  Response type: " <> typeName (Proxy @response)
               , "  Show'n response value: " <> show parsedBody
               , "  Show'n raw response body string: " <> show body
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

instance SubmissionResponse Data where
  parseResponse = fmap (demodulate . (show :: Bits -> String)) . readEither


-- * Helpers

httpLogRun :: BaseUrl -> Maybe PlayerKey -> Maybe ApiKey -> IO ()
httpLogRun baseUrl playerKey apiKey
  = errPutStrLn
  $ intercalate "\n"
  [ "ServerUrl: " <> show baseUrl
  , "PlayerKey: " <> maybe "(not set)" show playerKey
  , "ApiKey: "    <> maybe "(not set)" show apiKey
  ]
