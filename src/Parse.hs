-- |Cribbed from Stack Overflow just to ease expression writing a little
{-# LANGUAGE OverloadedStrings #-}
module Parse (parse, toExpr, Ast(..)) where

import           Types

data Ast x = Val x | Node [Ast x] deriving (Eq, Show)

parseh :: String -> String -> [Ast String] -> (Ast String, String)
parseh [] _ as = (Node as, [])
parseh (x : xs) w as
  | x == '$' = let (a, xs') = (parseh xs [] [])
               in (parseh xs' [] (as ++ [a]))
  | x == '^' = (Node (as ++ if length w > 0 then [Val w] else []), xs)
  | x == ' ' = parseh xs [] (as ++ if length w > 0 then [Val w] else [])
  | otherwise = parseh xs (w ++ [x]) as

parse :: [Char] -> [Ast [Char]]
parse xs = let (Node as, _) = parseh xs [] [] in as

toExpr :: Ast [Char] -> Expr
toExpr (Val x) = Ref $ Symbol x
toExpr (Node (Val "let" : Val s : x : y : _)) = Let (Symbol s) (toExpr x) (toExpr y)
toExpr (Node (Val "fn" : Val s : x : _)) = Lam (Symbol s) (toExpr x)
toExpr (Node (x : y : _)) = Call (toExpr x) (toExpr y)
