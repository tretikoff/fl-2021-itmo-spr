module Test.Parser.Prefix where

import Test.Tasty.HUnit (Assertion)

import Test.Common (syntaxError, parsingSuccess)

import Parser.Prefix (parse)
import Expr (Expr (..), plus, mult, pow)

unit_error :: Assertion
unit_error = do
  syntaxError parse "+123"
  syntaxError parse "1+23"
  syntaxError parse "12+3"
  syntaxError parse "12+"
  syntaxError parse "1+2"
  syntaxError parse ""

unit_success :: Assertion
unit_success = do
  parsingSuccess parse "123" (Num 123)
  parsingSuccess parse "+1 2" (plus (Num 1) (Num 2))
  parsingSuccess parse "+*1 2 3" (plus (mult (Num 1) (Num 2)) (Num 3))
  parsingSuccess parse "+1*2 3" (plus (Num 1) (mult (Num 2) (Num 3)))
  parsingSuccess parse "+1*2+3 4" (plus (Num 1) (mult (Num 2) (plus (Num 3) (Num 4))))
  parsingSuccess parse "+1*+2 3 4" (plus (Num 1) (mult (plus (Num 2) (Num 3)) (Num 4)))
  parsingSuccess parse "+1*^2 3 4" (plus (Num 1) (mult (pow (Num 2) (Num 3)) (Num 4)))
  parsingSuccess parse "+1^^2 3 4" (plus (Num 1) (pow (pow (Num 2) (Num 3)) (Num 4)))

unit_spaces :: Assertion
unit_spaces = do
  parsingSuccess parse "    +1*^2 3 4" (plus (Num 1) (mult (pow (Num 2) (Num 3)) (Num 4)))
  parsingSuccess parse "+1*^2 3 4    " (plus (Num 1) (mult (pow (Num 2) (Num 3)) (Num 4)))
  parsingSuccess parse "+ 1 * ^ 2 3 4" (plus (Num 1) (mult (pow (Num 2) (Num 3)) (Num 4)))
  parsingSuccess parse "+1*^2    3 4" (plus (Num 1) (mult (pow (Num 2) (Num 3)) (Num 4)))
  parsingSuccess parse "+1*^2 3  4" (plus (Num 1) (mult (pow (Num 2) (Num 3)) (Num 4)))
  parsingSuccess parse "+1 *\t^2 \n3 4" (plus (Num 1) (mult (pow (Num 2) (Num 3)) (Num 4)))
