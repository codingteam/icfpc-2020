module Evaluator (evaluateSymbol) where

import Data.IntMap ((!))
import Debug.Trace

import Reducer

getExpr :: DefId -> Program -> ExprTree
getExpr id program = program ! id

evaluateExprGuarded :: String -> ExprTree -> ExprTree -> Program -> ExprTree
evaluateExprGuarded label original new program =
  if original == new
  then original
  else trace ("# " ++ label ++ ": \norig: " ++ (show original) ++ "\nnew: " ++ (show new)) $
       evaluateExpr (simplify new) program

evaluateExpr :: ExprTree -> Program -> ExprTree
evaluateExpr orig@(DefValue v) p =
  let newExpr = getExpr v p in
  evaluateExprGuarded "(DefValue v)" orig newExpr p
evaluateExpr orig@(Ap (DefValue v) x) p =
  let newExpr = Ap (getExpr v p) x in
  evaluateExprGuarded "(Ap (DefValue v) x)" orig newExpr p
evaluateExpr orig@(Ap f (DefValue v)) p =
  let newExpr = Ap (simplify f) (getExpr v p) in
  evaluateExprGuarded "(Ap f (DefValue v))" orig newExpr p
evaluateExpr orig@(Ap f x) p =
  let newExpr = Ap (evaluateExpr f p) (evaluateExpr x p) in
  evaluateExprGuarded "(Ap f x)" orig newExpr p
evaluateExpr e _ = e

evaluateSymbol :: DefId -> Program -> ExprTree
evaluateSymbol id program =
  let expr = getExpr id program in
  trace (show expr) $
    evaluateExpr expr program
