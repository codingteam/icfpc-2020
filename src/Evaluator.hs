module Evaluator  where

import Data.IntMap ((!))
import Debug.Trace
import Data.Maybe

import Reducer

getExpr :: DefId -> Program -> ExprTree
getExpr id program = program ! id

evaluateExprGuarded :: String -> ExprTree -> ExprTree -> Program -> ExprTree
evaluateExprGuarded label original new program =
  trace ("# " ++ label ++ ": \norig: " ++ (show original) ++ "\nnew: " ++ (show new)) $
       let expr' = simplify new
       in fromMaybe expr' $ evaluateExpr (simplify new) program

evaluateExpr' :: ExprTree -> Program -> ExprTree
evaluateExpr' e p = fromMaybe e $ evaluateExpr e p

evaluateExpr :: ExprTree -> Program -> Maybe ExprTree
evaluateExpr orig@(DefValue v) p =
  let newExpr = getExpr v p in
  Just $ evaluateExprGuarded "(DefValue v)" orig newExpr p
evaluateExpr orig@(Ap (DefValue v) x) p =
  let newExpr = Ap (getExpr v p) x in
  Just $ evaluateExprGuarded "(Ap (DefValue v) x)" orig newExpr p
evaluateExpr orig@(Ap f (DefValue v)) p =
  let newExpr = Ap (simplify f) (getExpr v p) in
  Just $ evaluateExprGuarded "(Ap f (DefValue v))" orig newExpr p
evaluateExpr orig@(Ap f x) p = do
  f' <- evaluateExpr f p
  x' <- evaluateExpr x p
  let newExpr = Ap f' x' 
  Just $ evaluateExprGuarded "(Ap f x)" orig newExpr p
evaluateExpr e _ = Nothing

evaluateSymbol :: DefId -> Program -> ExprTree
evaluateSymbol id program =
  let expr = getExpr id program in
  trace (show expr) $
    fromMaybe expr $ evaluateExpr expr program
