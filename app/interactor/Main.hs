{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Exception (IOException, catch)
import Control.Monad ((<=<), join)

import System.Environment (getArgs)
import System.IO (readFile)

import Interactor
import Newtypes (ApiKey, parseApiKey)
import HttpApi (submission, sendMessageToAliens, httpLogRun)
import Constants
import Invaluator (Data)
import Helpers


usageInfo :: String
usageInfo = "Usage: interactor [<symbol> <filePath>] [<state> <dx> <dy>]"


main :: IO ()
main = do
  apiKey <- loadApiKey
  putStrLn $ "ApiKey = " ++ show apiKey
  baseUrl <- liftEither localBaseUrl

  let
    mkCommunicatior :: IO (Data -> IO Data)
    mkCommunicatior = do
      httpLogRun baseUrl Nothing apiKey
      pure (submission apiKey <=< sendMessageToAliens baseUrl)

  getArgs >>= \case
    [x] | x `elem` ["-h", "--help"] -> putStrLn usageInfo
    [] -> join $ multiShot <$> mkCommunicatior <*> pure defaultGalaxyFilePath
    [filePath] -> join $ multiShot <$> mkCommunicatior <*> pure filePath
    [state, dx, dy] -> oneShot "galaxy" "data/galaxy.txt" state dx dy
    [symbol, filePath, state, dx, dy] -> oneShot symbol filePath state dx dy
    _ -> fail usageInfo


loadApiKey :: IO (Maybe ApiKey)
loadApiKey = go where
  go = (extractApiKey =<<) <$> safeLoadFile' "secret"

  extractApiKey
    = either (const Nothing) Just
    . parseApiKey
    . filter (`notElem` "\n\r")


safeLoadFile' :: FilePath -> IO (Maybe String)
safeLoadFile' p =
  (Just <$> readFile p) `catch`
  (const (pure Nothing) :: IOException -> IO (Maybe String))
