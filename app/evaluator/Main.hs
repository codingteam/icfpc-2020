module Main where

import System.Environment (getArgs)

import Evaluator (evaluateSymbol)
import Reducer (parseProgram)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [symbol, filePath] -> do
      putStrLn $ "Reading file " ++ filePath ++ "..."
      fileContents <- readFile filePath
      putStrLn "Parsing program..."
      let program = parseProgram fileContents
      putStrLn $ "Evaluating symbol " ++ symbol ++ "..."
      let result = evaluateSymbol symbol program
      putStrLn $ symbol ++ ": " ++ (show result)
    _ -> putStrLn "Usage: evaluator <symbol> <filePath>"
