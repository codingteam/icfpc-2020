module Reducer (
    Token(..)
  , ExprTree(..)
  , Operation(..)
  , Program
  , DefId
  , reduce
  , parseProgram
  , simplifyProgram
  , simplify
  ) where

import Debug.Trace
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Text.Read (readMaybe)

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
  show Cdr = "cdr"
  show Cons = "cons"
  show Nil = "nil"
  show IsNil = "isnil"

type VarId = Int
type DefId = Int

type Program = IntMap ExprTree

data Definition = Definition DefId ExprTree

data ExprTree =
    Ap ExprTree ExprTree
  | Number Int
  | Op Operation
  | Var VarId
  | DefValue DefId
  | Lambda (ExprTree -> ExprTree)

instance Eq ExprTree where
  (Ap f1 x1) == (Ap f2 x2) = (f1 == f2) && (x1 == x2)
  (Number x1) == (Number x2) = x1 == x2
  (Op o1) == (Op o2) = o1 == o2
  (Var v1) == (Var v2) = v1 == v2
  (DefValue d1) == (DefValue d2) = d1 == d2
  (Lambda f1) == _ = error "Won't compare lambdas!"
  _ == (Lambda f2) = error "Won't compare lambdas!"
  _ == _ = False

instance Show ExprTree where
  show (Ap f x) = "ap " ++ show f ++ " " ++ show x
  show (Number x) = show x
  show (Op o) = show o
  show (Var v) = show v
  show (DefValue d) = ":" ++ show d
  show (Lambda _) = "Lambda"

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
  helper ("cdr":rest) = (Op Cdr, rest)
  helper ("cons":rest) = (Op Cons, rest)
  helper ("nil":rest) = (Op Nil, rest)
  helper ("isnil":rest) = (Op IsNil, rest)
  helper (('x':varid):rest)
    | Just varid' <- readMaybe varid = (Var varid', rest)
  helper ((':':defid):rest)
    | Just defid' <- readMaybe defid = (DefValue defid', rest)
  helper (number:rest)
    | Just number' <- readMaybe number = (Number number', rest)
  helper wtf = trace ("[helper" ++ show wtf ++ "]") undefined

parseDefintion :: [Token] -> Definition
parseDefintion ((':':defid):"=":rest) = Definition (read defid) (parse rest)
parseDefintion ("galaxy":"=":rest) = Definition 0 (parse rest)

parseProgram :: String -> Program
parseProgram = id
  . IntMap.fromList
  . map (\(Definition a b) -> (a, b))
  . map (parseDefintion . words)
  . lines

flatten :: ExprTree -> [Token]
flatten (Ap left right) = "ap" : (flatten left) ++ (flatten right)
flatten (Number i) = [show i]
flatten (Op op) = [show op]
flatten (Var varid) = ['x' : show varid]

simplify :: ExprTree -> ExprTree
simplify tree@(Ap left right) =
  let simplified = helper tree
  in case helper tree of
      Nothing ->
        case (helper left, helper right) of
          (Just left', Nothing) -> simplify (Ap left' right)
          (Nothing, Just right') -> simplify (Ap left right')
          (Just left', Just right') -> simplify (Ap left' right')
          (Nothing, Nothing) -> tree
      Just simplified -> simplify simplified
  where
  helper :: ExprTree -> Maybe ExprTree
  helper (Ap (Lambda f) x) = Just $ f x
  helper (Ap (Op Inc) (Number x)) = Just $ Number (x+1)

  helper (Ap (Op Dec) (Number x)) = Just $ Number (x-1)

  helper (Ap (Op Dec) (Ap (Op Inc) x)) = Just $ x
  helper (Ap (Op Inc) (Ap (Op Dec) x)) = Just $ x
  helper (Ap (Op Dec) (Ap (Ap (Op Add) x) (Number 1))) = Just $ x

  helper (Ap (Ap (Op Add) (Number 0)) y) = Just $ y
  helper (Ap (Ap (Op Add) x) (Number 0)) = Just $ x
  helper (Ap (Ap (Op Add) (Number x)) (Number y)) = Just $ Number (x+y)

  helper (Ap (Ap (Op Mul) (Number 0)) y) = Just $ Number 0
  helper (Ap (Ap (Op Mul) x) (Number 0)) = Just $ Number 0
  helper (Ap (Ap (Op Mul) (Number 1)) y) = Just $ y
  helper (Ap (Ap (Op Mul) x) (Number 1)) = Just $ x
  helper (Ap (Ap (Op Mul) (Number x)) (Number y)) = Just $ Number (x*y)

  helper (Ap (Ap (Op Div) x) (Number 1)) = Just $ x
  helper (Ap (Ap (Op Div) (Number x)) (Number y)) = Just $ Number (x `quot` y)

  helper (Ap (Ap (Op Equals) x) y)
    | x == y = Just $ Op Truthy
    | otherwise = Just $ Op Falsy

  helper (Ap (Ap (Op LessThan) (Number x)) (Number y))
    | x < y = Just $ Op Truthy
    | otherwise = Just $ Op Falsy

  helper (Ap (Op Negate) (Number x)) = Just $ Number (-x)

  helper (Ap (Ap (Ap (Op S) op1) op2) x) = Just $ Ap (Ap op1 x) (Ap op2 x)

  helper (Ap (Ap (Ap (Op C) op1) x) y) = Just $ Ap (Ap op1 y) x
  helper (Ap (Op C) x) =
    Just $
      Lambda $ \y -> Lambda $ \z ->
        let new = Ap (Ap x z) y
        in case helper new of
            Just ok -> ok
            Nothing -> new

  helper (Ap (Ap (Ap (Op B) x) y) z) = Just $ Ap x (Ap y z)

  helper (Ap (Op I) x) = Just $ x

  helper (Ap (Ap (Ap (Op Cons) x0) x1) x2) = Just $ Ap (Ap x2 x0) x1

  helper (Ap (Op Car) (Ap (Ap (Op Cons) x0) x1)) = Just $ x0

  helper (Ap (Op Car) x) = Just $ Ap x (Op Truthy)

  helper (Ap (Op Cdr) (Ap (Ap (Op Cons) x0) x1)) = Just $ x1

  helper (Ap (Op Cdr) x) = Just $ Ap x (Op Falsy)

  helper (Ap (Op Nil) _) = Just $ Op Truthy

  helper (Ap (Op IsNil) (Op Nil)) = Just $ Op Truthy
  helper (Ap (Op IsNil) _) = Just $ Op Falsy

  helper (Ap (Ap (Op Falsy) _) arg2) = Just $ arg2

  helper x = Nothing

simplify x = x

simplifyProgram :: Program -> Program
simplifyProgram = IntMap.map simplify
