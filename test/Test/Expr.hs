module Test.Expr where

import Test.Tasty.HUnit (Assertion, (@?=))

import Expr (plus, mult, pow, eval)
import AstStmt (Expr (..))

unit_eval :: Assertion
unit_eval = do
  eval (plus (Num 1) (Num 2)) @?= 1+2
  eval (plus (mult (Num 1) (Num 2)) (Num 3)) @?= 1*2+3
  eval (plus (Num 1) (mult (Num 2) (Num 3))) @?= 1+2*3
  eval (plus (plus (Num 1) (mult (Num 2) (Num 3))) (Num 4)) @?= 1+2*3+4
  eval (plus (Num 1) (mult (mult (Num 2) (Num 3)) (Num 4))) @?= 1+2*3*4
  eval (plus (mult (plus (Num 1) (Num 2)) (Num 3)) (Num 4)) @?= (1+2)*3+4
  eval (mult (plus (Num 1) (Num 2)) (pow (Num 3) (Num 4))) @?= (1+2)*3^4
  eval (plus (Num 1) (mult (Num 2) (plus (Num 3) (Num 4)))) @?= 1+2*(3+4)
  eval (plus (Num 1) (mult (Num 2) (pow (Num 3) (Num 4)))) @?= 1+2*(3^4)
  eval (pow (Num 2) (pow (Num 3) (Num 4))) @?= 2^3^4
  eval (pow (pow (Num 2) (Num 3)) (Num 4)) @?= (2^3)^4

