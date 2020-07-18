module Evaluator where

import Reducer

evaluateSymbol :: Token -> Program -> ExprTree
evaluateSymbol name program = Number 0
