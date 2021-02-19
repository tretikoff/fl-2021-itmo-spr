module Parser.Infix where

import Parser.Common (parseOp, parseDigit, parserEof)
import Control.Applicative ((<|>))
import Data.Char (isDigit, digitToInt, isSpace)
import Expr (Expr (..), Operator (..))
import Text.Printf (printf)
import Lexer (Token (..), lexer)

parse :: String -> Maybe Expr
parse = parserEof parseSum

-- Expr :: Expr + Expr
--       | Expr * Expr
--       | Expr ^ Expr
--       | Number
--       | ( Expr )
--Spaces :: Space [Space]

-- Expr :: Слаг + Слаг + ... + Слаг
--       = Слаг (+ Слаг) (+ Слаг) .. (+ Слаг)
--       -> [Слаг] - fold ... -> BinOp Plus (BinOp Plus ...)...
-- Слаг :: Множ * Множ * ... * Множ
-- Множ :: Фактор ^ Фактор ^ ... ^ Фактор
-- Фактор :: Цифра | ( Expr )
-- [1,2,3] -> (1+2)+3

data Associativity = AssocL | AssocR

binOp :: Associativity -> Operator -> [Expr] -> Expr
binOp AssocL op = foldl1 (BinOp op)
binOp AssocR op = foldr1 (BinOp op)

parseInfix :: Associativity -> Operator -> (String -> Maybe ([Token], b)) -> (String -> Maybe ([Token], Expr)) -> String -> Maybe ([Token], Expr)
parseInfix ass oper constr next str = do
    tokens <- lexer str
    go tokens
  where
    go (Oper op : t) = do
      (t', l) <- go t
      (t'', r) <- go t'
      return (t'', binOp op l r)
    go (Number number : t) = do
--      if null t return (t, Num number) else
      return (t, Num number)
    go _ = Nothing

spaces :: String -> Maybe (String, ())
spaces (h:t) | isSpace h = spaces t
spaces t = return (t, ())

parseSum :: String -> Maybe (String, Expr)
parseSum = parseInfix AssocL Plus (parseOp '+') parseMult

parseMult :: String -> Maybe (String, Expr)
parseMult = parseInfix AssocL Mult (parseOp '*') parsePow

parsePow :: String -> Maybe (String, Expr)
parsePow = parseInfix AssocR Pow (parseOp '^') (\str -> parseDigit str <|> parseExprBr str)

parseExprBr :: String -> Maybe (String, Expr)
parseExprBr ('(' : t) =
  case parseSum t of
    Just (')' : t', e) -> Just (t', e)
    _ -> Nothing
parseExprBr _ = Nothing


