{-# LANGUAGE LambdaCase, ViewPatterns, OverloadedStrings, NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.List (intercalate)

import Control.Monad ((<=<), forever, void)
import Control.Concurrent (threadDelay)

import System.Environment (getArgs)

import Newtypes
import Modulator
import HttpApi
import Helpers
import qualified Constants
import Production (production)
import Invaluator (Data)
import Bits (Bits)


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
  localBaseUrl <- liftEither Constants.localBaseUrl

  getArgs >>= \case
    [x] | x `elem` ["-h", "--help"] ->
      putStrLn usageInfo

    [ "--local", "send",
      parseApiKey -> apiKey,
      parsePlayerKey -> playerKey ] -> do

        apiKey'    <- liftEither apiKey
        playerKey' <- liftEither playerKey
        httpLogRun localBaseUrl (Just playerKey') (Just apiKey')

        (_ :: Bits) <-
          submission (Just apiKey') =<<
            sendMessageToAliens localBaseUrl
              (CallToAliens Join playerKey' Nothing Nothing)

        let thirdValue = UnknownYetThirdValue 5 10 15 20

        (_ :: Bits) <-
          submission (Just apiKey') =<<
            sendMessageToAliens localBaseUrl
              (CallToAliens Start playerKey' (Just thirdValue) Nothing)

        forever $ do
          (_ :: Bits) <-
            submission (Just apiKey') =<<
              sendMessageToAliens localBaseUrl
                (CallToAliens Commands playerKey' (Just thirdValue) Nothing)

          threadDelay 1_000_000

    [ "--local", "receive",
      parseApiKey -> apiKey,
      parseAliensResponseId -> responseId ] -> do

        apiKey' <- liftEither apiKey
        httpLogRun localBaseUrl Nothing (Just apiKey')

        void $ submission (Just apiKey')
          =<< getResponseFromAliens localBaseUrl
          =<< liftEither responseId

    -- !!! PRODUCTION !!! --
    ( (parseBaseUrl   -> baseUrl) :
      (parsePlayerKey -> playerKey) :
      (oneOrZero      -> Just (fmap parseApiKey -> apiKey)) ) -> do

        baseUrl'   <- liftEither baseUrl
        playerKey' <- liftEither playerKey
        apiKey'    <- maybe (pure Nothing) (fmap Just . liftEither) apiKey
        httpLogRun baseUrl' (Just playerKey') apiKey'

        let
          talkWithAliens :: Modulatable msg => msg -> IO Data
          talkWithAliens = submission apiKey' <=< sendMessageToAliens baseUrl'

        production playerKey' talkWithAliens

    args ->
      fail ("Unexpected arguments: " <> show args <> "\n" <> usageInfo)

  where
    oneOrZero :: [a] -> Maybe (Maybe a)
    oneOrZero [ ] = Just Nothing
    oneOrZero [a] = Just (Just a)
    oneOrZero  _  = Nothing
