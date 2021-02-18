module Test.Lexer where

import Test.Tasty.HUnit (Assertion)

import Test.Common (syntaxError, parsingSuccess)

import Lexer (lexer, Token (..))
import Expr (Operator (..))


unit_error :: Assertion
unit_error = do
  syntaxError lexer "12 1 3 !"
  syntaxError lexer "1+2;13;42"
  syntaxError lexer "1+a+3"
  syntaxError lexer "1-2"

unit_success :: Assertion
unit_success = do
  parsingSuccess lexer "+123"      [Oper Plus, Number 123]
  parsingSuccess lexer "12+"       [Number 12, Oper Plus]
  parsingSuccess lexer "+12"       [Oper Plus, Number 12]
  parsingSuccess lexer ""          []
  parsingSuccess lexer "123"       [Number 123]
  parsingSuccess lexer "1+23"      [Number 1,Oper Plus,Number 23]
  parsingSuccess lexer "12+3"      [Number 12,Oper Plus,Number 3]
  parsingSuccess lexer "1+2"       [Number 1,Oper Plus,Number 2]
  parsingSuccess lexer "1*2+3"     [Number 1,Oper Mult,Number 2,Oper Plus,Number 3]
  parsingSuccess lexer "1+2*3"     [Number 1,Oper Plus,Number 2,Oper Mult,Number 3]
  parsingSuccess lexer "1+2*3+4"   [Number 1,Oper Plus,Number 2,Oper Mult,Number 3,Oper Plus,Number 4]
  parsingSuccess lexer "1+2*3*4"   [Number 1,Oper Plus,Number 2,Oper Mult,Number 3,Oper Mult,Number 4]
  parsingSuccess lexer "(1+2)*3+4" [Lbr,Number 1,Oper Plus,Number 2,Rbr,Oper Mult,Number 3,Oper Plus,Number 4]
  parsingSuccess lexer "(1+2)*3^4" [Lbr,Number 1,Oper Plus,Number 2,Rbr,Oper Mult,Number 3,Oper Pow,Number 4]
  parsingSuccess lexer "1+2*(3+4)" [Number 1,Oper Plus,Number 2,Oper Mult,Lbr,Number 3,Oper Plus,Number 4,Rbr]
  parsingSuccess lexer "1+2*(3^4)" [Number 1,Oper Plus,Number 2,Oper Mult,Lbr,Number 3,Oper Pow,Number 4,Rbr]
  parsingSuccess lexer "1^2^3^4"   [Number 1,Oper Pow,Number 2,Oper Pow,Number 3,Oper Pow,Number 4]

unit_spaces :: Assertion
unit_spaces = do
  parsingSuccess lexer "   1+2*(3^4)"  [Number 1,Oper Plus,Number 2,Oper Mult,Lbr,Number 3,Oper Pow,Number 4,Rbr]
  parsingSuccess lexer "1+2*(3^4)    " [Number 1,Oper Plus,Number 2,Oper Mult,Lbr,Number 3,Oper Pow,Number 4,Rbr]
  parsingSuccess lexer "1+2*( 3  ^4)"  [Number 1,Oper Plus,Number 2,Oper Mult,Lbr,Number 3,Oper Pow,Number 4,Rbr]
  parsingSuccess lexer "1+2 * (3^4)"   [Number 1,Oper Plus,Number 2,Oper Mult,Lbr,Number 3,Oper Pow,Number 4,Rbr]
  parsingSuccess lexer "1 + 2 * (3^4)" [Number 1,Oper Plus,Number 2,Oper Mult,Lbr,Number 3,Oper Pow,Number 4,Rbr]
  parsingSuccess lexer "1\t+2\n*(3^4)" [Number 1,Oper Plus,Number 2,Oper Mult,Lbr,Number 3,Oper Pow,Number 4,Rbr]

unit_numbers :: Assertion
unit_numbers = do
  parsingSuccess lexer "   123+2*(13^42)"  [Number 123,Oper Plus,Number 2,Oper Mult,Lbr,Number 13,Oper Pow,Number 42,Rbr]
  parsingSuccess lexer "123+2*(13^42)    " [Number 123,Oper Plus,Number 2,Oper Mult,Lbr,Number 13,Oper Pow,Number 42,Rbr]
  parsingSuccess lexer "123+2*( 13  ^42)"  [Number 123,Oper Plus,Number 2,Oper Mult,Lbr,Number 13,Oper Pow,Number 42,Rbr]
  parsingSuccess lexer "123+2 * (13^42)"   [Number 123,Oper Plus,Number 2,Oper Mult,Lbr,Number 13,Oper Pow,Number 42,Rbr]
  parsingSuccess lexer "123 + 2 * (13^42)" [Number 123,Oper Plus,Number 2,Oper Mult,Lbr,Number 13,Oper Pow,Number 42,Rbr]
  parsingSuccess lexer "123\t+2\n*(13^42)" [Number 123,Oper Plus,Number 2,Oper Mult,Lbr,Number 13,Oper Pow,Number 42,Rbr]
