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

import IcfpcMmxx.Request
import IcfpcMmxx.Utils (prettyPrintJSON)


-- | WARNING! Save API key to this file before run!
apiKeyFile :: FilePath
apiKeyFile = "secret"


main :: IO ()
main = do
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


-- Initial prototype (kept in a comment just in case)
{-
import System.Environment
import Network.HTTP.Simple
import Data.ByteString.Lazy.UTF8 as BLU
import Control.Exception

main = catch (
    do
        args <- getArgs
        putStrLn ("ServerUrl: " ++ args!!0 ++ "; PlayerKey: " ++ args!!1)
        request' <- parseRequest ("POST " ++ (args!!0))
        let request = setRequestBodyLBS (BLU.fromString (args!!1)) request'
        response <- httpLBS request
        let statuscode = show (getResponseStatusCode response)
        case statuscode of
            "200" -> putStrLn ("Server response: " ++ BLU.toString (getResponseBody response))
            _ -> putStrLn ("Unexpected server response:\nHTTP code: " ++ statuscode ++ "\nResponse body: " ++ BLU.toString (getResponseBody response))
    ) handler
    where
        handler :: SomeException -> IO ()
        handler ex = putStrLn $ "Unexpected server response:\n" ++ show ex
-}