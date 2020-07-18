module Evaluator (
    getExpr
  , evaluateExpr
  , evaluateSymbol
) where

import Data.IntMap ((!))
import Debug.Trace
import Data.Maybe

import Reducer

getExpr :: DefId -> Program -> ExprTree
getExpr id program = program ! id

evaluateExpr :: ExprTree -> Program -> ExprTree
evaluateExpr e p = evaluate p e

evaluateSymbol :: DefId -> Program -> ExprTree
evaluateSymbol id program =
  let expr = getExpr id program in
  trace (show expr) $ evaluateExpr expr program
