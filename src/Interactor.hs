module Interactor
     ( oneShot
     , multiShot
     ) where

import Control.Monad (when)
import System.Exit (exitSuccess)
import System.IO (isEOF, hFlush, stdout)

import qualified Invaluator as I
import Modulator (printBits, modulate)
import Demodulator (demodulate)


oneShot :: String -> FilePath -> [Char] -> String -> String -> IO ()
oneShot symbol filePath state dx dy = do
  symbolValue <- I.loadSymbol filePath symbol
  state <- I.loadSymbolContents ("galaxy = " ++ state) "galaxy"
  state' <- I.evalData state
  result <- I.interact symbolValue state' (I.mkDVec (read dx) (read dy))
  let result' = I.alienShow result
  putStrLn $ "+++" ++ result'

multiShot :: FilePath -> IO b
multiShot filePath = do
  galaxy <- I.loadGalaxy filePath
  interactiveLoop galaxy I.DNil

interactiveLoop :: I.ExprRef -> I.Data -> IO a
interactiveLoop galaxy state = do
  eof <- isEOF
  when eof exitSuccess

  x:y:rest <- words <$> getLine
  let
    state' = case rest of
      [] -> state
      new -> I.alienParseData (unwords new)

  result@(I.InteractResult0 state'' _) <-
    loopInteract galaxy state' (I.mkDVec (read x) (read y))

  putStrLn $ "+++" ++ I.alienShow result
  hFlush stdout
  interactiveLoop galaxy state''

loopInteract :: I.ExprRef -> I.Data -> I.Data -> IO I.InteractResult
loopInteract galaxy state vec = do
  res <- I.interact galaxy state vec
  case res of
    I.InteractResult0 _ _ -> return res
    I.InteractResult1 num state' data_ -> do
      putStrLn $ "Sending " ++ (show data_) ++ "to server"
      putStrLn "(unimplemented yet)"
      hFlush stdout
      let dataToSend = printBits $ modulate data_ :: String
      serverReply <- undefined -- TODO: send data over HTTP and get response
      let result = demodulate serverReply
      loopInteract galaxy state' result
