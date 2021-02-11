module Test.Parser where

import Data.Maybe (isNothing, isJust, fromJust)
import Parser
import Test.Tasty.HUnit ((@?=), Assertion, assertBool)

syntaxError :: (String -> Maybe a) -> String -> Assertion
syntaxError parser input = assertBool "Expected syntax error" (isNothing $ parser input)

parsingSuccess :: (Show a, Eq a) => (String -> Maybe a) -> String -> a -> Assertion
parsingSuccess parser input exp = do
  let result = parser input
  assertBool "Expected parsing to succeed" (isJust result)
  fromJust result @?= exp

unit_parsePrefix :: Assertion
unit_parsePrefix = do
  let parser = parse Prefix
  syntaxError parser "123"
  syntaxError parser "+123"
  syntaxError parser "1+23"
  syntaxError parser "12+3"
  syntaxError parser "12+"
  syntaxError parser "1+2"
  syntaxError parser ""
  parsingSuccess parser "+12" (plus (Num 1) (Num 2))
  parsingSuccess parser "+*123" (plus (mult (Num 1) (Num 2)) (Num 3))
  parsingSuccess parser "+1*23" (plus (Num 1) (mult (Num 2) (Num 3)))
  parsingSuccess parser "+1*2+34" (plus (Num 1) (mult (Num 2) (plus (Num 3) (Num 4))))
  parsingSuccess parser "+1*+234" (plus (Num 1) (mult (plus (Num 2) (Num 3)) (Num 4)))
  parsingSuccess parser "+1*^234" (plus (Num 1) (mult (pow (Num 2) (Num 3)) (Num 4)))
  parsingSuccess parser "+1^^234" (plus (Num 1) (pow (pow (Num 2) (Num 3)) (Num 4)))

unit_parseInfix :: Assertion
unit_parseInfix = do
  let parser = parse Infix
  syntaxError parser "123"
  syntaxError parser "+123"
  syntaxError parser "1+23"
  syntaxError parser "12+3"
  syntaxError parser "12+"
  syntaxError parser "+12"
  syntaxError parser ""
  parsingSuccess parser "1+2" (plus (Num 1) (Num 2))
  parsingSuccess parser "1*2+3" (plus (mult (Num 1) (Num 2)) (Num 3))
  parsingSuccess parser "1+2*3" (plus (Num 1) (mult (Num 2) (Num 3)))
  parsingSuccess parser "1+2*3+4" (plus (plus (Num 1) (mult (Num 2) (Num 3))) (Num 4))
  parsingSuccess parser "1+2*3*4" (plus (Num 1) (mult (mult (Num 2) (Num 3)) (Num 4)))
  parsingSuccess parser "(1+2)*3+4" (plus (mult (plus (Num 1) (Num 2)) (Num 3)) (Num 4))
  parsingSuccess parser "(1+2)*3^4" (mult (plus (Num 1) (Num 2)) (pow (Num 3) (Num 4)))
  parsingSuccess parser "1+2*(3+4)" (plus (Num 1) (mult (Num 2) (plus (Num 3) (Num 4))))
  parsingSuccess parser "1+2*(3^4)" (plus (Num 1) (mult (Num 2) (pow (Num 3) (Num 4))))
  parsingSuccess parser "1^2^3^4" (pow (Num 1) (pow (Num 2) (pow (Num 3) (Num 4))))

