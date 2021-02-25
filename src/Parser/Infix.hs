{-# LANGUAGE LambdaCase #-}
module Parser.Infix where

import Expr ( Expr )
import Parser.Combinators ( Parser, eof, whitespaces, parseEither )

parse :: String -> Either String Expr
parse =
  parseEither (whitespaces *> parseExpr <* whitespaces <* eof)

parseExpr :: Parser String Expr
parseExpr = undefined