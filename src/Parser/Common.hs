module Parser.Common where

import Data.Char (isDigit, digitToInt)
import Expr (Operator (..), Expr (..), toOp)

parseOp :: Char -> String -> Maybe (String, Operator)
parseOp c (h : t) | c == h = Just (t, toOp c)
parseOp _ _ = Nothing

parseDigit :: String -> Maybe (String, Expr)
parseDigit (d : t) | isDigit d =
  Just (t, Num (digitToInt d))
parseDigit _ = Nothing

parserEof :: MonadFail m => (t -> m ([a], b)) -> t -> m b
parserEof parser str = do
  ([], r) <- parser str
  return r
