module Expr where

import AstStmt
import Text.Printf (printf)

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

eval :: Expr -> Int
eval (BinOp Pow l r) = eval l ^ eval r
eval (BinOp Mult l r) = eval l * eval r
eval (BinOp Plus l r) = eval l + eval r
eval (BinOp Minus l r) = eval l - eval r
eval (BinOp Eq l r) = if eval l == eval r then 1 else 0
eval (BinOp Neq l r) = if eval l /= eval r then 1 else 0
eval (BinOp Le l r) = if eval l <= eval r then 1 else 0
eval (BinOp Lt l r) = if eval l < eval r then 1 else 0
eval (BinOp Ge l r) = if eval l >= eval r then 1 else 0
eval (BinOp Gt l r) = if eval l > eval r then 1 else 0
eval (Num x) = x
eval (Ident var) = 0

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