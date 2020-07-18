module Evaluator (evaluateSymbol) where

import Data.IntMap ((!))

import Reducer

getExpr :: DefId -> Program -> ExprTree
getExpr id program = program ! id

evaluateSymbol :: DefId -> Program -> ExprTree
evaluateSymbol id program =
  let expr = getExpr id program in
  evaluate expr program
