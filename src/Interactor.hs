module Interactor
     ( oneShot
     , multiShot
     ) where

import Control.Monad (when)
import System.Exit (exitSuccess)
import System.IO (isEOF, hFlush, stdout)

import HttpApi (sendMessageDumb)
import Newtypes (ApiKey)
import qualified Invaluator as I


oneShot :: String -> FilePath -> [Char] -> String -> String -> IO ()
oneShot symbol filePath state dx dy = do
  symbolValue <- I.loadSymbol filePath symbol
  state <- I.loadSymbolContents ("galaxy = " ++ state) "galaxy"
  state' <- I.evalData state
  result <- I.interact symbolValue state' (I.mkDVec (read dx) (read dy))
  let result' = I.alienShow result
  putStrLn $ "+++" ++ result'

multiShot :: Maybe ApiKey -> FilePath -> IO b
multiShot apiKey filePath = do
  galaxy <- I.loadGalaxy filePath
  interactiveLoop apiKey galaxy I.DNil

interactiveLoop :: Maybe ApiKey -> I.ExprRef -> I.Data -> IO a
interactiveLoop apiKey galaxy state = do
  eof <- isEOF
  when eof exitSuccess

  x:y:rest <- words <$> getLine
  let
    state' = case rest of
      [] -> state
      new -> I.alienParseData (unwords new)

  result@(I.InteractResult0 state'' _) <-
    loopInteract apiKey galaxy state' (I.mkDVec (read x) (read y))

  putStrLn $ "+++" ++ I.alienShow result
  hFlush stdout
  interactiveLoop apiKey galaxy state''

loopInteract :: Maybe ApiKey -> I.ExprRef -> I.Data -> I.Data -> IO I.InteractResult
loopInteract apiKey galaxy state vec = do
  res <- I.interact galaxy state vec
  case res of
    I.InteractResult0 _ _ -> return res
    I.InteractResult1 num state' data_ -> do
      putStrLn $ "Sending " ++ (show data_) ++ "to server"
      putStrLn "(unimplemented yet)"
      hFlush stdout
      reply <- case apiKey of
        Just apiKey -> sendMessageDumb apiKey data_
        Nothing -> return undefined -- TODO: dummy placeholder value?
      loopInteract apiKey galaxy state' reply
