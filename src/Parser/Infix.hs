module Parser.Infix where

import Parser.Common (parseOp, parseDigit, parserEof)
import Control.Applicative ((<|>))
import Data.Char (isDigit, digitToInt, isSpace)
import Expr (Expr (..), Operator (..))
import Text.Printf (printf)

parse :: String -> Maybe Expr
parse = parserEof parseSum

-- Expr :: Expr + Expr
--       | Expr * Expr
--       | Expr ^ Expr
--       | Digit
--       | ( Expr )

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

parseBinOp :: Associativity -> Operator -> (String -> Maybe (String, b)) -> (String -> Maybe (String, Expr)) -> String -> Maybe (String, Expr)
parseBinOp assoc op parseOp nextParser str =
    (binOp assoc op <$>) <$> go str
  where
    go :: String -> Maybe (String, [Expr])
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

parseSum :: String -> Maybe (String, Expr)
parseSum = parseBinOp AssocL Plus (parseOp '+') parseMult

parseMult :: String -> Maybe (String, Expr)
parseMult = parseBinOp AssocL Mult (parseOp '*') parsePow

parsePow :: String -> Maybe (String, Expr)
parsePow = parseBinOp AssocR Pow (parseOp '^') (\str -> parseDigit str <|> parseExprBr str)

parseExprBr :: String -> Maybe (String, Expr)
parseExprBr ('(' : t) =
  case parseSum t of
    Just (')' : t', e) -> Just (t', e)
    _ -> Nothing
parseExprBr _ = Nothing


