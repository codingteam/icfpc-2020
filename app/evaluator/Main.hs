module Main where

import System.Environment (getArgs)
import Data.IORef

import Evaluator
import Reducer
import qualified Invaluator as I

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["galaxy", path, xs, ys] -> do
      let x = read xs
          y = read ys
      galaxyRef <- I.loadGalaxy path
      galaxyData <- I.interact galaxyRef I.DNil (I.mkDVec x y)
      print galaxyData

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
