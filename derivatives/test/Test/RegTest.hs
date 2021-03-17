module Test.RegTest where

import Regexp
import Test.HUnit (Assertion, assertBool)
import Control.DeepSeq (NFData(rnf))
import Data.Time.Clock

optDeriv :: Char -> Regexp -> Regexp
optDeriv _ Empty = Empty
optDeriv _ Epsilon = Empty
optDeriv c (Char r) | c == r = Epsilon
optDeriv c (Char r) = Empty
optDeriv c (Alt p q) = Alt (derivative c p) (derivative c q)
optDeriv c (Seq p q) | nullable p = Alt (Seq (optDeriv c p) q) (optDeriv c q)
optDeriv c (Seq p q) = if p == Empty || q == Empty then Empty else Seq (optDeriv c p) q
optDeriv c (Star r) = Seq (optDeriv c r) (Star r)


optMatch :: Regexp -> String -> Bool
optMatch r s = nullable (foldl (flip optDeriv) r s)

a :: Regexp
a = Char 'a'

b :: Regexp
b = Char 'b'

c :: Regexp
c = Char 'c'

-- a*a
r1 :: Regexp
r1 = Seq (Star a) a

-- (a|a)*
r2 :: Regexp
r2 = Star (Alt a a)

-- b | (c (a|b)*)
r3 :: Regexp
r3 = Alt b (Seq c (Star (Alt a b)))

unit_regexp :: Assertion
unit_regexp = do
  assertBool "a*a" (optMatch r1 (replicate 50 'a'))
  assertBool "a*a" (not $ optMatch r1 (replicate 50 'a' ++ "b"))

  assertBool "(a|a)*" (optMatch r2 (replicate 50 'a'))
  assertBool "(a|a)*" (not $ optMatch r2 (replicate 50 'a' ++ "b"))

  assertBool "b|c(a|b)*" (optMatch r3 "b")
  assertBool "b|c(a|b)*" (optMatch r3 "c")
  assertBool "b|c(a|b)*" (optMatch r3 "cabba")
  assertBool "b|c(a|b)*" (not $ optMatch r3 "d")
  assertBool "b|c(a|b)*" (not $ optMatch r3 "ba")
  assertBool "b|c(a|b)*" (not $ optMatch r3 "aaaa")

testPerformance = do
  start <- getCurrentTime
  let () = rnf (match r3 (replicate 1000000 'b'))
  end   <- getCurrentTime
  return $ diffUTCTime end start

testPerformanceOptimized = do
  start <- getCurrentTime
  let () = rnf (optMatch r3 (replicate 1000000 'b'))
  end   <- getCurrentTime
  return $ diffUTCTime end start

