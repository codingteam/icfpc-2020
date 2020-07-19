module Main where

import System.Environment (getArgs)

import qualified Invaluator as I

main :: IO ()
main = do
  args <- getArgs
  let actualArgs = case args of
                     [state, dx, dy] -> ["galaxy", "data/galaxy.txt", state, dx, dy]
                     full@[symbol, filePath, state, dx, dy] -> full
                     _ -> error "Usage: interactor [<symbol> <filePath>] <state> <dx> <dy>"
  let [symbol, filePath, state, dx, dy] = actualArgs
  symbolValue <- I.loadSymbol filePath symbol
  state <- I.loadSymbolContents ("galaxy = " ++ state) "galaxy"
  state' <- I.evalData state
  result <- I.interact symbolValue state' (read dx) (read dy)
  let result' = I.alienShow result
  putStrLn $ "+++" ++ result'
