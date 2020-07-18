
module Types where

type Token = String

data Operation =
    Add
  | Inc
  | Dec
  | Mul
  | Div
  | Equals
  | Truthy
  | Falsy
  | LessThan
  | Negate
  | S
  | C
  | B
  | I
  | Car
  | Cdr
  | Cons
  | Nil
  | IsNil
  deriving (Eq, Show)

data Expr =
    Ap Expr [Expr]
  | Op Operation
  | Number Integer
  deriving (Eq, Show)

