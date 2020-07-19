module Main where

import System.Environment (getArgs)

import Evaluator
import Reducer

main :: IO ()
main = do
  args <- getArgs
  let actualArgs = case args of
                     [dx, dy] -> ["0", "data/interactor.txt", dx, dy]
                     full@[symbol, filePath, dx, dy] -> full
                     _ -> error "Usage: interactor [<symbol> <filePath>] <dx> <dy>"
  let [symbol, filePath, dx, dy] = actualArgs
  fileContents <- readFile filePath
  let program = parseProgram fileContents
  let program' = simplifyProgram program
  let expr = getExpr (read symbol) program'
  let result = evaluateExpr expr program'
  putStrLn $ "+++" ++ show result
