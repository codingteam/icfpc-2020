module Main where

import System.Environment (getArgs)

import Evaluator (evaluateSymbol)
import Reducer (parseProgram, simplifyProgram)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [symbol, filePath] -> do
      putStrLn $ "Reading file " ++ filePath ++ "..."
      fileContents <- readFile filePath
      putStrLn "Parsing program..."
      let program = parseProgram fileContents
      putStrLn "Simplifying program..."
      let program' = simplifyProgram program
      putStrLn $ "Evaluating symbol " ++ symbol ++ "..."
      let result = evaluateSymbol (read symbol) program'
      putStrLn $ symbol ++ ": " ++ (show result)
    _ -> putStrLn "Usage: evaluator <symbol> <filePath>"
