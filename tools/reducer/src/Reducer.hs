module Reducer (
    Op(..)
  , reduce
  ) where

data Op =
    Number Int
  | Ap
  | Inc
  | Dec
  | Add
  deriving (Show, Eq)

reduce :: [Op] -> [Op]
reduce program = go [] (reverse program)
  where
  go stack [] = stack
  go (Inc:(Number value):stack) (Ap:rest) = go (Number (value + 1):stack) rest
  go (Dec:(Number value):stack) (Ap:rest) = go (Number (value - 1):stack) rest
  go (Add:(Number x):(Number y):stack) (Ap:Ap:rest) = go (Number (x+y):stack) rest
  go stack (op:rest) = go (op:stack) rest
