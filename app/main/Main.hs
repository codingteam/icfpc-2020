
module Main (main) where

import Data.ByteString.Lazy.UTF8 as BLU

import Control.Exception

import System.Environment
import System.Exit (die)
import System.IO (hPutStrLn, stderr)

import Network.HTTP.Simple

import Newtypes


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--local"] -> do
      baseUrl <- mkBaseUrl "https://icfpc2020-api.testkontur.ru"
      submissionMain baseUrl mempty

    [serverUrl, playerKey] -> do
      baseUrl <- mkBaseUrl serverUrl
      submissionMain baseUrl playerKey

    _ -> fail "Run either with --local, or server URL and playerKey"


getResponseFromAliens :: BaseUrl -> AliensResponseId -> IO Request
getResponseFromAliens baseUrl responseId =
  parseRequest $
    "GET " <> fromBaseUrl baseUrl <> "/aliens/" <>
    fromAliensResponseId responseId

sendMessageToAliens :: BaseUrl -> IO Request
sendMessageToAliens baseUrl =
  parseRequest $
    "POST " <> fromBaseUrl baseUrl <> "/aliens/send"


submissionMain :: BaseUrl -> String -> IO ()
submissionMain serverUrl playerKey = do
  hPutStrLn stderr $
    "ServerUrl: " <> show serverUrl <> "; " <>
    "PlayerKey: " <> show playerKey

  req <-
    getResponseFromAliens serverUrl =<<
      mkAliensResponseId "00112233-4455-6677-8899-aabbccddeeff"

  commitReq req `catch` exceptionHandler

  where
    exceptionHandler :: SomeException -> IO ()
    exceptionHandler = die . ("Unexpected server response:\n" <>) . show

    commitReq :: Request -> IO ()
    commitReq req = do
      response <- httpLBS $ setRequestBodyLBS (BLU.fromString playerKey) req

      case getResponseStatusCode response of
           200 ->
             putStrLn $
               "Server response: " <> BLU.toString (getResponseBody response)

           statusCode ->
             die $ mconcat
                 [ "Unexpected server response:\n"
                 , "  HTTP code: " <> show statusCode <> "\n"
                 , "  Response body: " <> BLU.toString (getResponseBody response)
                 ]
