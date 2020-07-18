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
evaluateExpr orig p = Just $ evaluate p orig

evaluateSymbol :: DefId -> Program -> ExprTree
evaluateSymbol id program =
  let expr = getExpr id program in
  trace (show expr) $
    fromMaybe expr $ evaluateExpr expr program
