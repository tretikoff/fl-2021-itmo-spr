{-# LANGUAGE LambdaCase #-}
module Parser.Infix where

import Parser.Combinators
import Parser.Common
import Control.Applicative ( Alternative((<|>)), many )
import Expr ( toOp )
import AstStmt

parse :: String -> Either String Expr
parse = parseEither (whitespaces *> parseExpr <* whitespaces <* eof)

pow1  = toOp <$> word "^"
power = (pow1, RightAssoc)

mult1  = toOp <$> word "*"
div1  = toOp <$> word "/"
mulDiv = ((mult1 <|> div1), LeftAssoc)
sum1  = toOp <$> word "+"
minus1 = toOp <$> word "-"
sumMin = ((sum1 <|> minus1), LeftAssoc)

eq = toOp <$> word "=="
neq = toOp <$> word "/="
leq = toOp <$> word "<="
lt = toOp <$> word "<"
geq = toOp <$> word ">="
gr = toOp <$> word ">"
equality = ((eq <|> neq  <|> leq <|> lt <|> geq <|> gr), NoAssoc)

ident = (:) <$> (alpha <|> underscore) <*> many (alpha <|> digit <|> underscore)

parseExpr :: Parser String Expr
parseExpr = uberExpr [equality, sumMin, mulDiv, power] (whitespaces *> local <* whitespaces) BinOp where
    local = Num <$> number <|> Ident <$> ident <|> brackets opBr clBr parseExpr