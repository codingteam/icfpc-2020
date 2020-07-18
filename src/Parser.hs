
module Parser where

import Types

parse :: [Token] -> Expr
parse = fst . helper
  where
  helper :: [Token] -> (Expr, [Token])
  helper ("ap":rest) =
    let (left, rest') = helper rest
        (right, rest'') = helper rest'
    in (Ap left [right], rest'')
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

