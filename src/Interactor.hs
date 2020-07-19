module Interactor
     ( oneShot
     , multiShot
     ) where

import Data.Function (fix)

import Control.Monad (when)

import System.Exit (exitSuccess)
import System.IO (isEOF, hFlush, stdout)

import HttpApi (sendMessageDumb)
import Newtypes (ApiKey)
import qualified Invaluator as I
import Modulator (modulate)
import Demodulator (demodulate)


oneShot :: String -> FilePath -> [Char] -> String -> String -> IO ()
oneShot symbol filePath state dx dy = do
  symbolValue <- I.loadSymbol filePath symbol
  state <- I.loadSymbolContents ("galaxy = " ++ state) "galaxy"
  state' <- I.evalData state
  result <- I.interact symbolValue state' (I.mkDVec (read dx) (read dy))
  let result' = I.alienShow result
  putStrLn $ "+++" ++ result'


multiShot :: (I.Data -> IO I.Data) -> FilePath -> IO b
multiShot talkWithAliens filePath = do
  galaxy <- I.loadGalaxy filePath
  interactiveLoop talkWithAliens galaxy I.DNil


interactiveLoop :: (I.Data -> IO I.Data) -> I.ExprRef -> I.Data -> IO a
interactiveLoop talkWithAliens galaxy = fix $ \again state -> do
  eof <- isEOF
  when eof exitSuccess

  x:y:rest <- words <$> getLine
  let
    state' = case rest of
      [] -> state
      new -> I.alienParseData (unwords new)

  result@(I.InteractResult0 state'' _) <-
    loopInteract talkWithAliens galaxy state' (I.mkDVec (read x) (read y))

  putStrLn $ "+++" ++ I.alienShow result
  hFlush stdout
  again state''


loopInteract
  :: (I.Data -> IO I.Data)
  -> I.ExprRef
  -> I.Data
  -> I.Data
  -> IO I.InteractResult

loopInteract talkWithAliens galaxy = fix $ \again state vec -> do
  res <- I.interact galaxy state vec

  case res of
    I.InteractResult0 _ _ -> return res
    I.InteractResult1 num state' data_ -> do
      putStrLn $ "Sending " ++ show data_ ++ "to server"
      putStrLn "(unimplemented yet)"
      hFlush stdout
      talkWithAliens data_ >>= again state'
