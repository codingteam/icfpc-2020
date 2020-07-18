module Main (main) where

import System.Environment (getArgs)

import Reducer (parseProgram)

main :: IO ()
main = do
  args <- getArgs
  putStrLn "Running parser"
  putStrLn $ show args
  case args of
    [filePath] -> do
      contents <- readFile filePath
      let result = parseProgram contents
      putStrLn $ show result
    _ -> do
      putStrLn "Usage: parser <filePath>"
