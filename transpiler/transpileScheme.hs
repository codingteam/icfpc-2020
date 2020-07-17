#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p
#! nix-shell "haskellPackages.ghcWithPackages (pkgs: with pkgs; [extra])"
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/5cb5ccb54229efd9a4cd1fccc0f43e0bbed81c5d.tar.gz

import Data.List (intercalate)
import Data.Tuple.Extra (first)
import System.Environment (getArgs)

data Expr = ELeaf String | ECall String [Expr]

instance Show Expr where
  show e = "'" ++ showExpr e ++ "'"

showValue ('-':x) = "(" ++ x ++ ")"
showValue (':':x) = "(f" ++ x ++ ")"
showValue x = x

showExpr (ELeaf x) = showValue x
showExpr (ECall x childs) =
  "(" ++ showValue x ++ " " ++ (intercalate " " $ map showExpr childs) ++ ")"

parse :: [String] -> Maybe (Expr, [String])
parse ("ap":list) = do
  let (aps, list') = first ((+1) . length) $ span (=="ap") list
  (op, list'') <- pHead list'
  (childs, list''') <- pRepeat parse aps list''
  Just $ (ECall op childs, list''')
parse (x:xs) = Just (ELeaf x, xs)
parse [] = Nothing

pHead :: [a] -> Maybe (a, [a])
pHead [] = Nothing
pHead (x:xs) = Just (x, xs)

pRepeat :: (aa -> Maybe (a, aa)) -> Int -> aa -> Maybe ([a], aa)
pRepeat _ 0 list = Just ([], list)
pRepeat f n list = do
  (x, list') <- f list
  (xs, list'') <- pRepeat f (n-1) list'
  Just (x:xs, list'')

parse' :: [String] -> Maybe (String, Expr)
parse' (ident: "=": list) = case parse list of
  Just (result, []) -> Just (ident, result)
  _ -> Nothing
parse' _ = Nothing

testData :: [[String]]
testData = map words
  [ "ap ap add 0 1"
  , "ap ap add 0 ap ap add 1 2"
  , "ap ap add ap ap add 0 1 ap ap add 2 3"
  ]

parseLines = unlines . map (maybe "err" show . parse')

translate = id
  . unlines
  . map (\(Just (a, b)) ->
          "(define (" ++ showValue a ++ ") " ++ showExpr b ++ ")")
  . map parse'
  . map words
  . lines

main = do
  [arg0] <- getArgs
  f <- readFile arg0
  putStrLn $ translate f
