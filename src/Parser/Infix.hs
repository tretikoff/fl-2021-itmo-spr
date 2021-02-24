{-# LANGUAGE LambdaCase #-}
module Parser.Infix where

import Parser.Common (parserEof, satisfy)
import Control.Applicative ((<|>))
import Expr (Expr (..), Operator (..))
import Lexer (Token (..), lexer)

parse :: String -> Maybe Expr
parse str = do
  tokens <- lexer str
  parserEof parseSum tokens

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


parseBinOp :: Associativity -> Operator -> ([Token] -> Maybe ([Token], b)) -> ([Token] -> Maybe ([Token], Expr)) -> [Token] -> Maybe ([Token], Expr)
parseBinOp assoc op parseOp nextParser str =
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

parseSum :: [Token] -> Maybe ([Token], Expr)
parseSum = parseBinOp AssocL Plus (satisfy (== (Oper Plus))) parseMult

parseMult :: [Token] -> Maybe ([Token], Expr)
parseMult = parseBinOp AssocL Mult  (satisfy (== (Oper Mult))) parsePow

parsePow :: [Token] -> Maybe ([Token], Expr)
parsePow = parseBinOp AssocR Pow (satisfy (== (Oper Pow))) (\str -> matchNum str <|> parseExprBr str)

matchNum :: [Token] -> Maybe ([Token], Expr)
matchNum (Number n : t) = return (t, Num n)
matchNum _ = Nothing

parseExprBr :: [Token] -> Maybe ([Token], Expr)
parseExprBr (Lbr : t) =
  case parseSum t of
    Just (Rbr : t', e) -> Just (t', e)
    _ -> Nothing
parseExprBr _ = Nothing


