module Reducer (
    Op(..)
  , reduce
  ) where

data Op =
    Number Int
  | Ap
  | Inc
  | Dec
  deriving (Show, Eq)

reduce :: [Op] -> [Op]
reduce program = go [] (reverse program)
  where
  go stack [] = stack
  go (Inc:(Number value):stack) (Ap:rest) = go (Number (value + 1):stack) rest
  go (Dec:(Number value):stack) (Ap:rest) = go (Number (value - 1):stack) rest
  go stack (op:rest) = go (op:stack) rest
