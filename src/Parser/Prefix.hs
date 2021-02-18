module Parser.Prefix where

import Parser.Common (parserEof)
import Expr (Expr (..), toOp)
import Data.Char ( isDigit, isSpace )
import Lexer (Token (..), lexer)

parse :: String -> Maybe Expr
parse = parserEof parsePrefix

-- scannerless parsing
parsePrefixScannerless :: String -> Maybe (String, Expr)
parsePrefixScannerless (op : tl) | op == '+' || op == '*' || op == '^' = do
  (t, _) <- spaces tl
  (t', l) <- parsePrefixScannerless t
  (t'', _) <- spaces t'
  (t''', r) <- parsePrefixScannerless t''
  return $ (t''', BinOp (toOp op) l r)
parsePrefixScannerless (d : t) | isDigit d = do
    (t', number) <- accumulateNumber [d] t
    return (t', Num (read number :: Int))
  where
    accumulateNumber acc (d:t) | isDigit d = accumulateNumber (d:acc) t
    accumulateNumber acc t = return (t, reverse acc)
parsePrefixScannerless _ = Nothing

spaces :: String -> Maybe (String, ())
spaces (h:t) | isSpace h = spaces t
spaces t = return (t, ())

-- lexer then parser
parsePrefix :: String -> Maybe ([Token], Expr)
parsePrefix str = do
    tokens <- lexer str
    go tokens
  where
    go (Oper op : t) = do
      (t', l) <- go t
      (t'', r) <- go t'
      return (t'', BinOp op l r)
    go (Number number : t) = do
      return (t, Num number)
    go _ = Nothing