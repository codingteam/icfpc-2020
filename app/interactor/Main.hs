module Main where

import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (isEOF, hPutStrLn, stderr, hFlush, stdout)

import qualified Invaluator as I

main :: IO ()
main = do
  args <- getArgs
  case args of
   [] -> multiShot "data/galaxy.txt"
   [filePath] -> multiShot filePath
   [state, dx, dy] -> oneShot "galaxy" "data/galaxy.txt" state dx dy
   [symbol, filePath, state, dx, dy] -> oneShot symbol filePath state dx dy
   _ -> error "Usage: interactor [<symbol> <filePath>] [<state> <dx> <dy>]"

oneShot symbol filePath state dx dy = do
  symbolValue <- I.loadSymbol filePath symbol
  state <- I.loadSymbolContents ("galaxy = " ++ state) "galaxy"
  state' <- I.evalData state
  result <- I.interact symbolValue state' (read dx) (read dy)
  let result' = I.alienShow result
  putStrLn $ "+++" ++ result'

multiShot filePath = do
  galaxy <- I.loadGalaxy filePath
  interactiveLoop galaxy I.DNil


errPutStrLn :: String -> IO ()
errPutStrLn = hPutStrLn stderr

interactiveLoop galaxy state = do
  eof <- isEOF
  when eof exitSuccess
  [x, y] <- (map read . words) <$> getLine
  result@(I.InteractResult _ state' _) <- I.interact galaxy state x y
  putStrLn $ "+++" ++ I.alienShow result
  hFlush stdout
  interactiveLoop galaxy state'
