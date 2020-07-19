{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (liftM2, forM)
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.List (elemIndex)
import Text.Read (readMaybe)
import qualified Data.HashMap.Strict as HashMap
import System.IO.Unsafe
import Data.Maybe
import Data.String (IsString, fromString)

data Expr
  = Ap !(IORef Expr) !(IORef Expr)
  | Builtin !BuiltinOp
  | Num !Integer

--------------------------------------------------------------------------------
-- Parser

parse :: IO (IORef Expr)
parse = do
  contents <- readFile "../data/galaxy.txt"

  let contents' = (map (words) . lines) contents

  -- Create a map with dummy references for each definition
  references <- fmap HashMap.fromList $ forM contents' $ \(name:"=":_) -> do
    ref <- newIORef (Num 0)
    return (name, ref)

  -- Replace these dummy references with parsed expressions
  forM contents' $ \(name:"=":def) ->
    parseLine (references HashMap.!) def
    >>= readIORef
    >>= writeIORef (references HashMap.! name)

  return $ (references HashMap.! "galaxy")

parseLine :: (String -> IORef Expr) -> [String] -> IO (IORef Expr)
parseLine gibe words = fst <$> p words
  where
  p :: [String] -> IO (IORef Expr, [String])
  p ("ap":xs) = do
    (a, xs') <- p xs
    (b, xs'') <- p xs'
    ap <- newIORef (Ap a b)
    return $ (ap, xs'')

  p (fun@(':':what):xs) = return (gibe fun, xs)

  p (num:xs) | Just num <- readMaybe num = do
    num' <- newIORef (Num num)
    return $ (num', xs)

  p (x:xs) = do
    x' <- newIORef (Builtin (fromJust $ parseBuiltinOp x))
    return $ (x', xs)

showExpr :: Int -> Expr -> IO String
showExpr l _ | l <= 0 = return "…"
showExpr _ (Num x) = return (show x)
showExpr _ (Builtin x) = return $ show x
showExpr l (Ap x y) = do
  x' <- showExpr (l-1) =<< readIORef x
  y' <- showExpr (l-1) =<< readIORef y
  return $ "(" ++ x' ++ " " ++ y' ++ ")"

sh' x = putStrLn =<< showExpr 10 =<< readIORef x
sh x = putStrLn =<< showExpr 10 x


--------------------------------------------------------------------------------
-- Galaxy

main = do
  galaxy <- parse

  app <-
      (mkApM
        (mkApM
           (pure galaxy)
           (newIORef (Builtin "nil")))
        (mkApM
          (mkApM
            (newIORef (Builtin "cons"))
            (newIORef (Num 0)))
          (newIORef (Num 0))))

  putStr "init: "
  sh' app

  putStr "next: "
  eval app
  sh' app

--------------------------------------------------------------------------------
-- Evaluator

eval :: IORef Expr -> IO Expr
eval ref = do
  oldVal <- readIORef ref
  newVal <- step oldVal
  case newVal of
    Nothing -> return oldVal
    Just v -> do
      writeIORef ref v
      eval ref

evalNum :: IORef Expr -> IO Integer
evalNum ref = do
  a <- eval ref
  case a of
    Num a -> return a
    e -> do
      x <- showExpr 5 e
      error $ "Bad number " ++ x

-- expr = Ap (       f1       ) x
-- f1   =     Ap (   f2   ) y
-- f2   =         Ap f3 z
step :: Expr -> IO (Maybe Expr)
step expr = case expr of
  Ap f1 x -> do
    f1' <- eval f1
    case f1' of
      Builtin "neg"   -> j $ Num . (0-) <$> evalNum x
      Builtin "i"     -> j $ readIORef x
      Builtin "nil"   -> j $ return $ toBool True
      Builtin "isnil" -> j $ Ap x <$> mkApM t (mkApM t f)
      Builtin "car"   -> j $ Ap x <$> t
      Builtin "cdr"   -> j $ Ap x <$> f
      Ap l2 y -> do
        l2' <- eval l2
        case l2' of
          Builtin "t"    -> j $ readIORef y
          Builtin "f"    -> j $ readIORef x
          Builtin "add"  -> j $ Num <$> liftM2 (+) (evalNum y) (evalNum x)
          Builtin "mul"  -> j $ Num <$> liftM2 (*) (evalNum y) (evalNum x)
          Builtin "div"  -> j $ Num <$> liftM2 div (evalNum y) (evalNum x)
          Builtin "lt"   -> j $ toBool <$> liftM2 (<)  (evalNum y) (evalNum x)
          Builtin "eq"   -> j $ toBool <$> liftM2 (==) (evalNum y) (evalNum x)
          -- Builtin "cons" -> ???
          Ap l3 z -> do
            l3' <- eval l3
            case l3' of
              Builtin "s"    -> j $ liftM2 Ap (mkAp z x) (mkAp y x)
              Builtin "c"    -> j $ liftM2 Ap (mkAp z x) (pure y)
              Builtin "b"    -> j $ liftM2 Ap (pure z)   (mkAp y x)
              Builtin "cons" -> j $ liftM2 Ap (mkAp x z) (pure y)
              _ -> return Nothing
          _ -> return Nothing
      _ -> return Nothing
  _ -> return Nothing
  where
  j = fmap Just
  t = newIORef (Builtin "t")
  f = newIORef (Builtin "f")

mkAp a b = newIORef (Ap a b)

mkApM a b = do
  a' <- a
  b' <- b
  mkAp a' b'

toBool True  = Builtin "t"
toBool False = Builtin "f"

--------------------------------------------------------------------------------
-- BuiltinOp

newtype BuiltinOp = BuiltinOp Int deriving (Eq)

instance Show BuiltinOp where
  show (BuiltinOp x) = builtinOps !! x

instance IsString BuiltinOp where
  fromString = fromJust . parseBuiltinOp

parseBuiltinOp :: String -> Maybe BuiltinOp
parseBuiltinOp x = BuiltinOp <$> elemIndex x builtinOps
builtinOps = ["neg","i","nil","isnil","car","cdr","t","f","add","mul","div","lt","eq","s","c","b","cons"]
