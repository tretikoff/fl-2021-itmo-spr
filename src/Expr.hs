module Expr where

import Text.Printf (printf)

data Operator = Pow
              | Mult
              | Div
              | Plus
              | Minus
              | Eq
              | Neq
              | Le
              | Lt
              | Ge
              | Gt
              deriving (Show, Eq)

toOp :: String -> Operator
toOp "^" = Pow
toOp "*" = Mult
toOp "/" = Div
toOp "+" = Plus
toOp "-" = Minus
toOp "==" = Eq
toOp "/=" = Neq
toOp "<=" = Le
toOp "<" = Lt
toOp ">=" = Ge
toOp ">" = Gt
toOp s = error $ printf "Unsupported operator: %s" s

data Expr = BinOp Operator Expr Expr
          | Num Int
          deriving (Show, Eq)

eval :: Expr -> Int
eval (BinOp Plus l r) = eval l + eval r
eval (BinOp Mult l r) = eval l * eval r
eval (BinOp Pow l r) = eval l ^ eval r
eval (Num x) = x

pow = BinOp Pow
mult = BinOp Mult
div' = BinOp Div
plus = BinOp Plus
minus = BinOp Minus
eq' = BinOp Eq
neq = BinOp Neq
le = BinOp Le
lt = BinOp Lt
ge = BinOp Ge
gt = BinOp Gt