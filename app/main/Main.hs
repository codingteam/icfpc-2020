{-# LANGUAGE LambdaCase, ViewPatterns, OverloadedStrings, NumericUnderscores #-}

module Main (main) where

import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.List (intercalate)
import Data.Functor ((<&>))
import Data.String (fromString)
import Data.Maybe (fromMaybe)

import Control.Exception
import Control.Monad (forever)
import Control.Concurrent (threadDelay)

import System.Environment
import System.IO (hPutStrLn, stderr)

import Network.HTTP.Simple

import Newtypes
import Modulator


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
  , "  main <base-url> <player-key> [<api-key>]"
  , "    Production-ish run (<api-key> is optional)"
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

        submission (Just apiKey') =<<
          sendMessageToAliens localBaseUrl
            (CallToAliens Join playerKey' Nothing Nothing)

        let thirdValue = UnknownYetThirdValue 5 10 15 20

        submission (Just apiKey') =<<
          sendMessageToAliens localBaseUrl
            (CallToAliens Start playerKey' (Just thirdValue) Nothing)

        forever $ do
          submission (Just apiKey') =<<
            sendMessageToAliens localBaseUrl
              (CallToAliens Commands playerKey' (Just thirdValue) Nothing)

          threadDelay 1_000_000

    [ "--local", "receive",
      parseApiKey -> apiKey,
      parseAliensResponseId -> responseId ] -> do

        apiKey' <- liftEither apiKey
        logRun localBaseUrl Nothing (Just apiKey')

        submission (Just apiKey') =<<
          getResponseFromAliens localBaseUrl =<< liftEither responseId

    ( (parseBaseUrl   -> baseUrl) :
      (parsePlayerKey -> playerKey) :
      (oneOrZero      -> Just (fmap parseApiKey -> apiKey)) ) -> do

        baseUrl'   <- liftEither baseUrl
        playerKey' <- liftEither playerKey
        apiKey'    <- maybe (pure Nothing) (fmap Just . liftEither) apiKey
        logRun baseUrl' (Just playerKey') apiKey'
        production baseUrl' playerKey' apiKey'

    args ->
      fail ("Unexpected arguments: " <> show args <> "\n" <> usageInfo)

  where
    oneOrZero :: [a] -> Maybe (Maybe a)
    oneOrZero [ ] = Just Nothing
    oneOrZero [a] = Just (Just a)
    oneOrZero  _  = Nothing


-- | TODO implement whatever logic is needed for production
production :: BaseUrl -> PlayerKey -> Maybe ApiKey -> IO ()
production baseUrl playerKey apiKey =
  submission apiKey =<<
    getResponseFromAliens baseUrl =<< liftEither
      (parseAliensResponseId "00112233-4455-6677-8899-aabbccddeeff")


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
