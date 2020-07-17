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
  | Cons
  deriving (Eq)

instance Show Operation where
  show Add = "add"
  show Inc = "inc"
  show Dec = "dec"
  show Mul = "mul"
  show Div = "div"
  show Equals = "eq"
  show Truthy = "t"
  show Falsy = "f"
  show LessThan = "lt"
  show Negate = "neg"
  show S = "s"
  show C = "c"
  show B = "b"
  show I = "i"
  show Car = "car"
  show Cons = "cons"

type VarId = Int

data ExprTree =
    Ap ExprTree ExprTree
  | Number Int
  | Op Operation
  | Var VarId
  deriving (Show, Eq)

reduce :: [Token] -> [Token]
reduce = flatten . simplify . parse

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
  helper ("div":rest) = (Op Div, rest)
  helper ("eq":rest) = (Op Equals, rest)
  helper ("t":rest) = (Op Truthy, rest)
  helper ("f":rest) = (Op Falsy, rest)
  helper ("lt":rest) = (Op LessThan, rest)
  helper ("neg":rest) = (Op Negate, rest)
  helper ("s":rest) = (Op S, rest)
  helper ("c":rest) = (Op C, rest)
  helper ("b":rest) = (Op B, rest)
  helper ("i":rest) = (Op I, rest)
  helper ("car":rest) = (Op Car, rest)
  helper ("cons":rest) = (Op Cons, rest)
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
simplify tree@(Ap left right) =
  let simplified = helper tree
  in if simplified == tree
        then let left' = simplify left
                 right' = simplify right
              in if (left' /= left) || (right' /= right)
                then simplify (Ap left' right')
                else helper tree
        else simplify simplified
  where
  helper (Ap (Op Inc) (Number x)) = Number (x+1)

  helper (Ap (Op Dec) (Number x)) = Number (x-1)

  helper (Ap (Op Dec) (Ap (Op Inc) x)) = x
  helper (Ap (Op Inc) (Ap (Op Dec) x)) = x
  helper (Ap (Op Dec) (Ap (Ap (Op Add) x) (Number 1))) = x

  helper (Ap (Ap (Op Add) (Number 0)) y) = y
  helper (Ap (Ap (Op Add) x) (Number 0)) = x
  helper (Ap (Ap (Op Add) (Number x)) (Number y)) = Number (x+y)

  helper (Ap (Ap (Op Mul) (Number 0)) y) = Number 0
  helper (Ap (Ap (Op Mul) x) (Number 0)) = Number 0
  helper (Ap (Ap (Op Mul) (Number 1)) y) = y
  helper (Ap (Ap (Op Mul) x) (Number 1)) = x
  helper (Ap (Ap (Op Mul) (Number x)) (Number y)) = Number (x*y)

  helper (Ap (Ap (Op Div) x) (Number 1)) = x
  helper (Ap (Ap (Op Div) (Number x)) (Number y)) = Number (x `quot` y)

  helper (Ap (Ap (Op Equals) x) y)
    | x == y = Op Truthy
    | otherwise = Op Falsy

  helper (Ap (Ap (Op LessThan) (Number x)) (Number y))
    | x < y = Op Truthy
    | otherwise = Op Falsy

  helper (Ap (Op Negate) (Number x)) = Number (-x)

  helper (Ap (Ap (Ap (Op S) op1) op2) x) = Ap (Ap op1 x) (Ap op2 x)

  helper (Ap (Ap (Ap (Op C) op1) x) y) = Ap (Ap op1 y) x

  helper (Ap (Ap (Ap (Op B) x) y) z) = Ap x (Ap y z)

  helper (Ap (Op I) x) = x

  helper (Ap (Op Car) (Ap (Ap (Op Cons) x0) x1)) = x0

  helper (Ap (Ap (Ap (Op Cons) x0) x1) x2) = Ap (Ap x2 x0) x1

  helper (Ap (Op Car) x) = Ap x (Op Truthy)

  helper x = x
simplify x = x
