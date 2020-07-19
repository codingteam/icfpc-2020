module Main where

import System.Environment (getArgs)

import Invaluator
import Data.IORef

main :: IO ()
main = do
  args <- getArgs
  let actualArgs = case args of
                     [state, dx, dy] -> ["galaxy", "data/galaxy.txt", state, dx, dy]
                     full@[symbol, filePath, state, dx, dy] -> full
                     _ -> error "Usage: interactor [<symbol> <filePath>] <state> <dx> <dy>"
  let [symbol, filePath, state, dx, dy] = actualArgs
  symbolValue <- loadSymbol filePath symbol
  state <- loadSymbolContents ("galaxy = " ++ state) "galaxy"
  state' <- evalData state
  result <- interactRaw symbolValue state' (read dx) (read dy)
  let result' = alienShowData result
  putStrLn $ "+++" ++ result'
