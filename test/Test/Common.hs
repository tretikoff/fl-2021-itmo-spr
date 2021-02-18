module Test.Common where

import Data.Maybe (isNothing, isJust, fromJust)
import Test.Tasty.HUnit ((@?=), Assertion, assertBool)

syntaxError :: (String -> Maybe a) -> String -> Assertion
syntaxError parser input = assertBool "Expected syntax error" (isNothing $ parser input)

parsingSuccess :: (Show a, Eq a) => (String -> Maybe a) -> String -> a -> Assertion
parsingSuccess parser input exp = do
  let result = parser input
  assertBool "Expected parsing to succeed" (isJust result)
  fromJust result @?= exp