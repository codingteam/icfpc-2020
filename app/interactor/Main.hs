{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.Environment (getArgs)

import Interactor


main :: IO ()
main =
  getArgs >>= \case
    [] -> multiShot "data/galaxy.txt"
    [filePath] -> multiShot filePath
    [state, dx, dy] -> oneShot "galaxy" "data/galaxy.txt" state dx dy
    [symbol, filePath, state, dx, dy] -> oneShot symbol filePath state dx dy
    _ -> fail "Usage: interactor [<symbol> <filePath>] [<state> <dx> <dy>]"
