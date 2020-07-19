{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.Environment (getArgs)
import Control.Exception(IOException, catch)
import System.IO (readFile)

import Interactor
import Newtypes (parseApiKey)


main :: IO ()
main = do
  apiKey <- loadApiKey
  putStrLn $ "ApiKey = " ++ show apiKey
  getArgs >>= \case
    [] -> multiShot apiKey "data/galaxy.txt"
    [filePath] -> multiShot apiKey filePath
    [state, dx, dy] -> oneShot "galaxy" "data/galaxy.txt" state dx dy
    [symbol, filePath, state, dx, dy] -> oneShot symbol filePath state dx dy
    _ -> fail "Usage: interactor [<symbol> <filePath>] [<state> <dx> <dy>]"

loadApiKey = do
  apiKey <- safeLoadFile' "secret"
  return $
    case apiKey of
      Nothing -> Nothing
      Just x -> case parseApiKey (filter (\x -> x /= '\n' && x /= '\r') x) of
        Left _ -> Nothing
        Right a -> Just a

safeLoadFile' :: FilePath -> IO (Maybe String)
safeLoadFile' p =
    (Just <$> readFile p) `catch`
    ((\e -> pure Nothing) :: IOException -> IO (Maybe String))
