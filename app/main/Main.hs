{-# LANGUAGE LambdaCase, ViewPatterns, OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.List (intercalate)
import Data.Functor ((<&>))
import Data.String (fromString)
import Data.Maybe (fromMaybe)

import Control.Exception
import Control.Monad (join)

import System.Environment
import System.IO (hPutStrLn, stderr)

import Network.HTTP.Simple

import Newtypes


usageInfo :: String
usageInfo
  = intercalate "\n"
  [ ""
  , "Usage info:"
  , "  main (-h|--help)"
  , "    Show this usage info"
  , ""
  , "  main --local send <api-key> <player-key>"
  , "    Send message to aliens via global server"
  , ""
  , "  main --local receive <api-key> <respone-id-uuid>"
  , "    Request message from aliens via global server"
  , ""
  , "  main <base-url> <player-key>"
  , "    Production-ish run"
  , ""
  ]


main :: IO ()
main = do
  localBaseUrl <-
    liftEither (parseBaseUrl "https://icfpc2020-api.testkontur.ru")

  getArgs >>= \case
    [x] | x `elem` ["-h", "--help"] ->
      putStrLn usageInfo

    [ "--local", "send",
      parseApiKey -> apiKey,
      parsePlayerKey -> playerKey ] -> do
        apiKey'    <- liftEither apiKey
        playerKey' <- liftEither playerKey
        logRun localBaseUrl (Just playerKey') (Just apiKey')
        req <- sendMessageToAliens localBaseUrl playerKey'
        submission req (Just apiKey')

    [ "--local", "receive",
      parseApiKey -> apiKey,
      parseAliensResponseId -> responseId ] -> do
        apiKey' <- liftEither apiKey
        logRun localBaseUrl Nothing (Just apiKey')
        req <- getResponseFromAliens localBaseUrl =<< liftEither responseId
        submission req (Just apiKey')

    [parseBaseUrl -> baseUrl, parsePlayerKey -> playerKey] -> do
      baseUrl'   <- liftEither baseUrl
      playerKey' <- liftEither playerKey
      logRun baseUrl' (Just playerKey') Nothing

      -- TODO implement whatever logic is needed for production

      req <-
        getResponseFromAliens baseUrl' =<< liftEither
          (parseAliensResponseId "00112233-4455-6677-8899-aabbccddeeff")

      submission req Nothing

    args ->
      fail ("Unexpected arguments: " <> show args <> "\n" <> usageInfo)


getResponseFromAliens :: BaseUrl -> AliensResponseId -> IO Request
getResponseFromAliens baseUrl responseId =
  parseRequest $
    "GET " <> fromBaseUrl baseUrl <> "/aliens/" <>
    fromAliensResponseId responseId

-- | TODO implement encoding (“modulate”) for request-body
sendMessageToAliens :: BaseUrl -> PlayerKey -> IO Request
sendMessageToAliens baseUrl playerKey =
  parseRequest ("POST " <> fromBaseUrl baseUrl <> "/aliens/send")
    <&> setRequestBodyLBS (BLU.fromString $ show $ fromPlayerKey playerKey)


submission :: Request -> Maybe ApiKey -> IO ()
submission request apiKey =
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

errPutStrLn :: String -> IO ()
errPutStrLn = hPutStrLn stderr

liftEither :: Either String a -> IO a
liftEither = either fail pure

logRun :: BaseUrl -> Maybe PlayerKey -> Maybe ApiKey -> IO ()
logRun baseUrl playerKey apiKey
  = errPutStrLn
  $ intercalate "\n"
  [ "ServerUrl: " <> show baseUrl
  , "PlayerKey: " <> maybe "(not set)" show playerKey
  , "ApiKey: "    <> maybe "(not set)" show apiKey
  ]
