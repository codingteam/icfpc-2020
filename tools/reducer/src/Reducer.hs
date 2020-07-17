module Reducer (
    Token(..)
  , ExprTree(..)
  , reduce
  ) where

type Token = String

data Operation =
    Add
  | Inc
  | Dec
  | Mul
  deriving (Eq)

instance Show Operation where
  show Add = "add"
  show Inc = "inc"
  show Dec = "dec"
  show Mul = "mul"

type VarId = Int

data ExprTree =
    Ap ExprTree ExprTree
  | Number Int
  | Op Operation
  | Var VarId
  deriving (Show, Eq)

parse :: [Token] -> ExprTree
parse = fst . helper
  where
  helper :: [Token] -> (ExprTree, [Token])
  helper ("ap":rest) =
    let (left, rest') = helper rest
        (right, rest'') = helper rest'
    in (Ap left right, rest'')
  helper ("inc":rest) = (Op Inc, rest)
  helper ("dec":rest) = (Op Dec, rest)
  helper ("add":rest) = (Op Add, rest)
  helper ("mul":rest) = (Op Mul, rest)
  -- XXX: `read` can fail, but we assume that the input is well-formed
  helper (('x':varid):rest) = (Var (read varid), rest)
  -- XXX: `read` can fail, but we assume that the input is well-formed
  helper (number:rest) = (Number (read number), rest)

flatten :: ExprTree -> [Token]
flatten (Ap left right) = "ap" : (flatten left) ++ (flatten right)
flatten (Number i) = [show i]
flatten (Op op) = [show op]
flatten (Var varid) = ['x' : show varid]

simplify :: ExprTree -> ExprTree
simplify (Ap (Op Inc) (Number x)) = Number (x+1)
simplify (Ap (Op Dec) (Number x)) = Number (x-1)
simplify (Ap (Ap (Op Add) (Number 0)) y) = y
simplify (Ap (Ap (Op Add) x) (Number 0)) = x
simplify (Ap (Ap (Op Add) (Number x)) (Number y)) = Number (x+y)
simplify (Ap (Ap (Op Mul) (Number 0)) y) = Number 0
simplify (Ap (Ap (Op Mul) x) (Number 0)) = Number 0
simplify (Ap (Ap (Op Mul) (Number 1)) y) = y
simplify (Ap (Ap (Op Mul) x) (Number 1)) = x
simplify (Ap (Ap (Op Mul) (Number x)) (Number y)) = Number (x*y)
simplify x = x

reduce :: [Token] -> [Token]
reduce = flatten . simplify . parse
