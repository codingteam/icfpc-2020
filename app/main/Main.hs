{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, BangPatterns #-}

module Main (main) where

import qualified Data.Text as T (filter)
import qualified Data.Text.IO as T (readFile)

import Control.Monad ((>=>), guard)
import Control.Monad.Catch (MonadThrow (throwM))

import System.Exit (die)

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Servant.Client
     ( ClientError
     , ClientM
     , parseBaseUrl
     , mkClientEnv
     , runClientM
     )

import System.Environment
import Network.HTTP.Simple
import Data.ByteString.Lazy.UTF8 as BLU
import Control.Exception

import IcfpcMmxx.Request
import IcfpcMmxx.Utils (prettyPrintJSON)


-- | WARNING! Save API key to this file before run!
apiKeyFile :: FilePath
apiKeyFile = "secret"


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--local"] -> localMain
    [serverUrl, playerKey] -> submissionMain serverUrl playerKey
    _ -> error "Run either with --local, or server URL and playerKey"


localMain :: IO()
localMain = do
  !playerKey <- do
    x <- T.filter (/= '\n') <$> T.readFile apiKeyFile
    x <$ guard (x /= mempty)

  -- Feed this function with a request monad from "IcfpcMmxx.Request" module.
  (req :: ClientM a -> IO a) <- do
    clientEnv <-
      mkClientEnv
        <$> newManager tlsManagerSettings
        <*> parseBaseUrl "https://icfpc2020-api.testkontur.ru"

    pure (flip runClientM clientEnv >=> either throwM pure)

  -- Pipe this app to “jq” in order to get colored output
  -- (in case you’re printing JSON into stdout of course).

  req getScoreboard >>= prettyPrintJSON

submissionMain :: String -> String -> IO ()
submissionMain serverUrl playerKey = catch (
    do
        putStrLn ("ServerUrl: " ++ serverUrl ++ "; PlayerKey: " ++ playerKey)
        request' <- parseRequest ("POST " ++ serverUrl)
        let request = setRequestBodyLBS (BLU.fromString playerKey) request'
        response <- httpLBS request
        let statuscode = show (getResponseStatusCode response)
        case statuscode of
            "200" -> putStrLn ("Server response: " ++ BLU.toString (getResponseBody response))
            _ -> putStrLn ("Unexpected server response:\nHTTP code: " ++ statuscode ++ "\nResponse body: " ++ BLU.toString (getResponseBody response))
    ) handler
    where
        handler :: SomeException -> IO ()
        handler ex = putStrLn $ "Unexpected server response:\n" ++ show ex
