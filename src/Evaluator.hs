module Evaluator (evaluateSymbol) where

import Data.IntMap ((!))
import Debug.Trace

import Reducer

getExpr :: DefId -> Program -> ExprTree
getExpr id program = program ! id

evaluateExpr :: ExprTree -> Program -> ExprTree
evaluateExpr (DefValue v) p =
  let newExpr = traceShowId $ getExpr v p in
  trace "# (DefValue v)" $ evaluateExpr newExpr p
evaluateExpr (Ap (DefValue v) x) p =
  let newExpr = traceShowId $ Ap (getExpr v p) x in
  trace "# (Ap (DefValue v) x)" $ evaluateExpr (simplify newExpr) p
evaluateExpr (Ap f (DefValue v)) p =
  let newExpr = traceShowId $ Ap f (getExpr v p) in
  trace "# (Ap f (DefValue v))" $ evaluateExpr (simplify newExpr) p
evaluateExpr e _ = e

evaluateSymbol :: DefId -> Program -> ExprTree
evaluateSymbol id program =
  let expr = getExpr id program in
  evaluateExpr expr program
