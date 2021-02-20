module Parser.Infix where

import Parser.Common (parserEof)
import Control.Applicative ((<|>))
import Data.Char (isDigit, digitToInt, isSpace)
import Expr (Expr (..), Operator (..))
import Text.Printf (printf)
import Lexer (Token (..), lexer)

parse :: String -> Maybe Expr
parse str = do
    x <- lexer str
    parserEof parseSum x

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

parseInfix :: Associativity -> Operator -> ([Token] -> Maybe ([Token], b)) -> ([Token] -> Maybe ([Token], Expr)) -> [Token] -> Maybe ([Token], Expr)
parseInfix assoc op parseOp nextParser str =
    (binOp assoc op <$>) <$> go str
  where
    go :: [Token] -> Maybe ([Token], [Expr])
    go str = do
      first@(t, e) <- nextParser str
      if null t
      then return (t, [e])
      else
        ( do
          (t', _) <- parseOp t
          let rest = go t'
          ((e:) <$>) <$> rest
        )
        <|>
        return (t, [e])

spaces :: String -> Maybe (String, ())
spaces (h:t) | isSpace h = spaces t
spaces t = return (t, ())

parseDigit :: [Token] -> Maybe ([Token], Expr)
parseDigit ((Number a): xs) = Just (xs, Num a)
parseDigit _ = Nothing

parseOp :: Operator -> [Token] -> Maybe ([Token], Operator)
parseOp op (x: xs) = if x == (Oper op) then Just (xs, op) else Nothing

parseSum :: [Token] -> Maybe ([Token], Expr)
parseSum = parseInfix AssocL Plus (parseOp Plus) parseMult

parseMult :: [Token] -> Maybe ([Token], Expr)
parseMult = parseInfix AssocL Mult (parseOp Mult) parsePow

parsePow :: [Token] -> Maybe ([Token], Expr)
parsePow = parseInfix AssocR Pow (parseOp Pow) (\str -> parseDigit str <|> parseExprBr str)

parseExprBr :: [Token] -> Maybe ([Token], Expr)
parseExprBr (Lbr : t) =
  case parseSum t of
    Just (Rbr : t', e) -> Just (t', e)
    _ -> Nothing
parseExprBr _ = Nothing


