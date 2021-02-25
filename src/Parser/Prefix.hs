module Parser.Prefix where

import Expr (Expr (..), Operator, toOp)
import Parser.Combinators
    ( Parser, eof, whitespaces, parseEither, word )
import Parser.Common ( number )
import Control.Applicative ( Alternative((<|>)) )

parse :: String -> Either String Expr
parse = parseEither (whitespaces *> parsePrefix <* whitespaces <* eof)

parseOp :: Parser String Operator
parseOp = toOp <$> (word "+" <|> word "*" <|> word "^")

parsePrefix :: Parser String Expr
parsePrefix =
  ( Num <$> number ) <|>
  ( do
    op <- parseOp
    whitespaces
    l <- parsePrefix
    whitespaces
    r <- parsePrefix
    return (BinOp op l r)
  )