{-# LANGUAGE OverloadedStrings #-}

module Invaluator
( ExprRef
, Data(..)
, InteractResult(..)
, loadGalaxy
, loadSymbol
, loadSymbolContents
, interact
, alienShow
, evalData
) where

import Control.Monad (liftM2, forM)
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.List (elemIndex)
import Text.Read (readMaybe)
import qualified Data.HashMap.Strict as HashMap
import System.IO.Unsafe
import Data.Maybe
import Data.String (IsString, fromString)
import Prelude hiding (interact)

--------------------------------------------------------------------------------
-- Public data

type ExprRef = IORef Expr

data Data = DCons Data Data | DNum Integer | DNil

data InteractResult = InteractResult Integer Data [[(Integer, Integer)]]
  deriving Show

--------------------------------------------------------------------------------
-- Internal data

data Expr
  = Ap !(ExprRef) !(ExprRef)
  | Builtin !BuiltinOp
  | Num !Integer

data ExprData
  = EDCons (ExprRef) (ExprRef)
  | EDNil
  | EDNum Integer

--------------------------------------------------------------------------------
-- Parser

loadGalaxy :: FilePath -> IO (ExprRef)
loadGalaxy path = loadSymbol path "galaxy"

loadSymbol :: FilePath -> String -> IO (ExprRef)
loadSymbol path symbol = do
  contents <- readFile path
  loadSymbolContents contents symbol

loadSymbolContents :: String -> String -> IO (ExprRef)
loadSymbolContents contents symbol = do
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

  return $ (references HashMap.! symbol)

parseLine :: (String -> ExprRef) -> [String] -> IO (ExprRef)
parseLine gibe words = fst <$> p words
  where
  p :: [String] -> IO (ExprRef, [String])
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

--------------------------------------------------------------------------------
-- Main

decodeInteractResult :: Data -> InteractResult
decodeInteractResult (DCons (DNum num) (DCons state (DCons img DNil))) =
  InteractResult num state (decodeImgs img)
  where
    -- TODO: handle decode errors?
    decodeImgs DNil = []
    decodeImgs (DCons x xs) = decodeImg x : decodeImgs xs

    decodeImg DNil = []
    decodeImg (DCons (DCons (DNum a) (DNum b)) xs) = (a, b) : decodeImg xs

encodeData :: Data -> IO (ExprRef)
encodeData (DNum x) = newIORef (Num x)
encodeData (DCons a b) =
  mkApM (mkApM (newIORef (Builtin "cons"))
               (encodeData a))
        (encodeData b)
encodeData DNil = newIORef (Builtin "nil")

interact :: ExprRef -> Data -> Integer -> Integer -> IO InteractResult
interact galaxy state x y = do
  expr <-
    (mkApM
      (mkApM (pure galaxy) (encodeData state))
      (mkApM
        (mkApM
          (newIORef (Builtin "cons"))
          (newIORef (Num x)))
        (newIORef (Num y))))

  data_ <- evalData expr
  return $ decodeInteractResult data_

--------------------------------------------------------------------------------
-- Evaluator

eval :: ExprRef -> IO Expr
eval ref = do
  oldVal <- readIORef ref
  newVal <- step oldVal
  case newVal of
    Nothing -> return oldVal
    Just v -> do
      writeIORef ref v
      eval ref

evalNum :: ExprRef -> IO Integer
evalNum ref = do
  a <- eval ref
  case a of
    Num a -> return a
    e -> do
      x <- showExpr 5 e
      error $ "Bad number " ++ x


evalExprData :: ExprRef -> IO (ExprData)
evalExprData ref = do
  -- ((cons a) b)
  ab <- eval ref
  case ab of
    Ap cons_a b -> do
      cons_a' <- eval cons_a
      case cons_a' of
        Ap cons a -> do
          cons' <- eval cons
          case cons' of
            Builtin "cons" -> return $ EDCons a b
    Builtin "nil" -> return EDNil
    Num x -> return (EDNum x)

evalData :: ExprRef -> IO Data
evalData ref = do
  c <- evalExprData ref
  case c of
    EDNil -> return DNil
    EDCons a b -> liftM2 DCons (evalData a) (evalData b)
    EDNum x -> return $ DNum x

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

--------------------------------------------------------------------------------
-- Show

showExpr :: Int -> Expr -> IO String
showExpr l _ | l <= 0 = return "…"
showExpr _ (Num x) = return (show x)
showExpr _ (Builtin x) = return $ show x
showExpr l (Ap x y) = do
  x' <- showExpr (l-1) =<< readIORef x
  y' <- showExpr (l-1) =<< readIORef y
  return $ "(" ++ x' ++ " " ++ y' ++ ")"

alienShowExpr :: Expr -> IO String
alienShowExpr (Num x) = return $ show x
alienShowExpr (Builtin x) = return $ show x
alienShowExpr (Ap f x) = do
  f' <- alienShowExpr =<< readIORef f
  x' <- alienShowExpr =<< readIORef x
  return $ "ap " ++ f' ++ " " ++ x'

class AlienShow a where
  alienShow :: a -> String

instance AlienShow Integer where
  alienShow x = show x

instance (AlienShow a, AlienShow b) => AlienShow (a, b) where
  alienShow (a, b) = "ap ap cons " ++ alienShow a ++ " " ++ alienShow b

instance AlienShow a => AlienShow [a] where
  alienShow [] = "nil"
  alienShow (x:xs) = alienShow (x, xs)

instance AlienShow Data where
  alienShow (DNum x) = show x
  alienShow (DCons a b) = alienShow (a, b)
  alienShow DNil = "nil"

instance AlienShow InteractResult where
  alienShow (InteractResult a b c) = alienShow (a, (b, (c, []::[Integer])))

instance Show Expr where
  show (Num x) = show x
  show (Builtin x) = show x
  show (Ap _ _) = "(… …)"

instance Show Data where
  show (DNum x) = show x
  show (DCons a b) = "(" ++ show a ++ "," ++ show b ++ ")"
  show (DNil) = "nil"

printExpr x = putStrLn =<< showExpr 10 x

printExprRef x = putStrLn =<< showExpr 10 =<< readIORef x

--------------------------------------------------------------------------------
-- ghci stuff

sh' = printExprRef

sh = printExpr
