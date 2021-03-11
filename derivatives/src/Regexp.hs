module Regexp where

import Prelude hiding (Seq)

data Regexp = Empty
            | Epsilon
            | Char Char
            | Seq Regexp Regexp
            | Alt Regexp Regexp
            | Star Regexp
            deriving (Eq, Ord)

match :: Regexp -> String -> Bool
match r s = nullable (foldl (flip derivative) r s)

nullable :: Regexp -> Bool
nullable Empty     = False
nullable Epsilon   = True
nullable (Char _)  = False
nullable (Alt p q) = nullable p || nullable q
nullable (Seq p q) = nullable p && nullable q
nullable (Star _)  = True

derivative :: Char -> Regexp -> Regexp
derivative _ Empty = Empty
derivative _ Epsilon = Empty
derivative c (Char r) | c == r = Epsilon
derivative c (Char r) = Empty
derivative c (Alt p q) = Alt (derivative c p) (derivative c q)
derivative c (Seq p q) | nullable p = Alt (Seq (derivative c p) q) (derivative c q)
derivative c (Seq p q) = Seq (derivative c p) q
derivative c (Star r) = Seq (derivative c r) (Star r)