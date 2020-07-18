module Main where

import System.Environment (getArgs)

import Evaluator
import Reducer

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["galaxy", filePath, xs, ys] -> do
      let symbol = 1338
      fileContents <- readFile filePath
      let program = parseProgram fileContents
      let program' = simplifyProgram program
      let galaxy = getExpr symbol program'
      let x = read xs
          y = read ys
      let expr = Ap (Ap galaxy (Op Nil)) (Ap (Ap (Op Cons) (Number x)) (Number y))
      print expr
      let result = evaluateExpr' expr program'
      putStrLn $ show result

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
