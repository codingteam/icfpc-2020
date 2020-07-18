module Evaluator where

import Control.Monad.State
import Types

-- not working actually...

type UniqueId = Int

data GlobalState = GlobalState
  { gsProg    :: Map String ExprTree
  , gsMemory  :: Map Ptr Value
  , gsCounter :: UniqueId
  }
  deriving (Show)

data Error = ETooFewArgs

type EvalM a = State GlobalState a

numArgs :: Operation -> Int
numArgs Add = 2
numArgs Inc = 1
numArgs Dec = 1
numArgs Mul = 2
numArgs Equals = 2
numArgs Truthy = 0
numArgs Falsy = 0
numArgs LessThan = 2
numArgs Negate = 1
numArgs S = 3
numArgs C = 3
numArgs B = 3
numArgs I = 1
numArgs Car = 1
numArgs Cdr = 1
numArgs Cons = 2
numArgs Nil = 0
numArgs IsNil = 1

eval :: Expr -> Expr
eval (Ap f []) = f
eval (Ap (Ap f xs) ys) = eval $ Ap f (xs ++ ys)
eval e@(Ap (Op op) xs) =
  if length xs == numArgs op
    then evalOp op xs
    else if length xs > numArgs op
           then error "too many args"
           else e
eval e = e

