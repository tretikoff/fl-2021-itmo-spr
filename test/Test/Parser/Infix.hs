module Test.Parser.Infix where

import Test.Tasty.HUnit (Assertion)

import Test.Common (syntaxError, parsingSuccess)

import Parser.Infix (parse)
import Expr (Expr (..), plus, mult, pow)


unit_error :: Assertion
unit_error = do
  syntaxError parse "123"
  syntaxError parse "+123"
  syntaxError parse "1+23"
  syntaxError parse "12+3"
  syntaxError parse "12+"
  syntaxError parse "+12"
  syntaxError parse ""

unit_success :: Assertion
unit_success = do
  parsingSuccess parse "1+2" (plus (Num 1) (Num 2))
  parsingSuccess parse "1*2+3" (plus (mult (Num 1) (Num 2)) (Num 3))
  parsingSuccess parse "1+2*3" (plus (Num 1) (mult (Num 2) (Num 3)))
  parsingSuccess parse "1+2*3+4" (plus (plus (Num 1) (mult (Num 2) (Num 3))) (Num 4))
  parsingSuccess parse "1+2*3*4" (plus (Num 1) (mult (mult (Num 2) (Num 3)) (Num 4)))
  parsingSuccess parse "(1+2)*3+4" (plus (mult (plus (Num 1) (Num 2)) (Num 3)) (Num 4))
  parsingSuccess parse "(1+2)*3^4" (mult (plus (Num 1) (Num 2)) (pow (Num 3) (Num 4)))
  parsingSuccess parse "1+2*(3+4)" (plus (Num 1) (mult (Num 2) (plus (Num 3) (Num 4))))
  parsingSuccess parse "1+2*(3^4)" (plus (Num 1) (mult (Num 2) (pow (Num 3) (Num 4))))
  parsingSuccess parse "1^2^3^4" (pow (Num 1) (pow (Num 2) (pow (Num 3) (Num 4))))

