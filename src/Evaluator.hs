module Evaluator (evaluateSymbol) where

import Data.IntMap ((!))

import Reducer

getExpr :: DefId -> Program -> ExprTree
getExpr id program = program ! id

evaluateExpr :: ExprTree -> Program -> ExprTree
evaluateExpr (Number x) _ = Number x
evaluateExpr (Ap (DefValue v) x) p =
  let newExpr = Ap (getExpr v p) x in
  simplify newExpr  
evaluateExpr e _ = error $ "Cannot evaluate expression " ++ (show e)

evaluateSymbol :: DefId -> Program -> ExprTree
evaluateSymbol id program =
  let expr = getExpr id program in
  evaluateExpr expr program
