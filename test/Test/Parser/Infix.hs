module Test.Parser.Infix where

import Test.Tasty.HUnit (Assertion)

import Test.Common (syntaxError, parsingSuccess)

import Parser.Infix (parse)
import Expr
    ( div',
      eq',
      ge,
      gt,
      le,
      lt,
      minus,
      mult,
      neq,
      plus,
      pow,
      Expr(..) )


unit_error :: Assertion
unit_error = do
  syntaxError parse "+123"
  syntaxError parse "12+"
  syntaxError parse "+12"
  syntaxError parse ""

unit_success :: Assertion
unit_success = do
  parsingSuccess parse "123" (Num 123)
  parsingSuccess parse "1+23" (plus (Num 1) (Num 23))
  parsingSuccess parse "12+3" (plus (Num 12) (Num 3))
  parsingSuccess parse "1+2" (plus (Num 1) (Num 2))
  parsingSuccess parse "1*2+3" (plus (mult (Num 1) (Num 2)) (Num 3))
  parsingSuccess parse "1+2*3" (plus (Num 1) (mult (Num 2) (Num 3)))
  parsingSuccess parse "1+2*3+4" (plus (plus (Num 1) (mult (Num 2) (Num 3))) (Num 4))
  parsingSuccess parse "1+2*3*4" (plus (Num 1) (mult (mult (Num 2) (Num 3)) (Num 4)))
  parsingSuccess parse "1-2/3/4" (minus (Num 1) (div' (div' (Num 2) (Num 3)) (Num 4)))
  parsingSuccess parse "(1+2)*3+4" (plus (mult (plus (Num 1) (Num 2)) (Num 3)) (Num 4))
  parsingSuccess parse "(1+2)*3^4" (mult (plus (Num 1) (Num 2)) (pow (Num 3) (Num 4)))
  parsingSuccess parse "1+2*(3+4)" (plus (Num 1) (mult (Num 2) (plus (Num 3) (Num 4))))
  parsingSuccess parse "1+2*(3^4)" (plus (Num 1) (mult (Num 2) (pow (Num 3) (Num 4))))
  parsingSuccess parse "1^2^3^4" (pow (Num 1) (pow (Num 2) (pow (Num 3) (Num 4))))
  parsingSuccess parse "1^2==3+4*5" (eq' (pow (Num 1) (Num 2)) (plus (Num 3) (mult (Num 4) (Num 5))))

unit_idents :: Assertion
unit_idents = do
  parsingSuccess parse "x + 13" (plus (Ident "x") (Num 13))
  parsingSuccess parse "var_42 + 13" (plus (Ident "var_42") (Num 13))
  parsingSuccess parse "camelCase + 13" (plus (Ident "camelCase") (Num 13))


unit_idents_error :: Assertion
unit_idents_error = do
  syntaxError parse "123abc"
  syntaxError parse "13+$x"

unit_spaces :: Assertion
unit_spaces = do
  parsingSuccess parse "1 +\t2*(3^4)"   (plus (Num 1) (mult (Num 2) (pow (Num 3) (Num 4))))
  parsingSuccess parse "1+2*(3^4)"      (plus (Num 1) (mult (Num 2) (pow (Num 3) (Num 4))))
  parsingSuccess parse "1+2*( 3  ^4)"   (plus (Num 1) (mult (Num 2) (pow (Num 3) (Num 4))))
  parsingSuccess parse "1+2 * (3^4)"    (plus (Num 1) (mult (Num 2) (pow (Num 3) (Num 4))))
  parsingSuccess parse "1 + 2 * (3^4)"  (plus (Num 1) (mult (Num 2) (pow (Num 3) (Num 4))))
  parsingSuccess parse "1\t+2\n*(3^4)"  (plus (Num 1) (mult (Num 2) (pow (Num 3) (Num 4))))

unit_nonassoc :: Assertion
unit_nonassoc = do
  parsingSuccess parse "1==2" (eq' (Num 1) (Num 2))
  parsingSuccess parse "1/=2" (neq (Num 1) (Num 2))
  parsingSuccess parse "1<=2" (le (Num 1) (Num 2))
  parsingSuccess parse "1<2" (lt (Num 1) (Num 2))
  parsingSuccess parse "1>=2" (ge (Num 1) (Num 2))
  parsingSuccess parse "1>2" (gt (Num 1) (Num 2))
  parsingSuccess parse "1==2" (eq' (Num 1) (Num 2))

unit_nonassoc_error :: Assertion
unit_nonassoc_error = do
  syntaxError parse "1=2"
  syntaxError parse "1==2==3"
  syntaxError parse "1<2>3"
  syntaxError parse "1<=2==3"
  syntaxError parse "1==2>=3"
  syntaxError parse "1==2/=3"
  syntaxError parse "1===2"







