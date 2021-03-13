module Parser.Common where

import Data.Char (isDigit, isAlpha)
import Parser.Combinators ( Parser, satisfy, symbol)
import Control.Applicative (some, Alternative((<|>)) )

digit :: Parser String Char
digit = satisfy isDigit

digit' :: Parser String Char
digit' = satisfy (\x -> isDigit x && x /= '0')

alpha :: Parser String Char
alpha = satisfy isAlpha

opBr = symbol '('
clBr = symbol ')'

underscore :: Parser String Char
underscore = symbol '_'

alphaUn :: Parser String Char
alphaUn = alpha <|> underscore


alphaNumUn :: Parser String Char
alphaNumUn = alpha <|> digit <|> underscore

-- number :: not empty, consists of digits
number :: Parser String Int
number = do
  num <- some digit
  return $ (read num :: Int)

data Associativity = LeftAssoc  -- x `op` y `op` z == (x `op` y) `op` z
                   | RightAssoc -- x `op` y `op` z == x `op` (y `op` z)
                   | NoAssoc    -- non associative operation:
                                -- x `op` y -- ok
                                -- x `op` y `op` z -- not ok

uberExpr :: [(Parser i op, Associativity)]
         -> Parser i ast
         -> (op -> ast -> ast -> ast)
         -> Parser i ast
uberExpr ((parse, ass): parsers) elemParser createAst = local ass <|> parseRest where
    parseRest = uberExpr parsers elemParser createAst
    local LeftAssoc = parseAssoc go where
        go expr ((op, opExpr): opExprs) = go (createAst op expr opExpr) opExprs
        go e [] = e
    local RightAssoc = parseAssoc go where
        go expr ((op, opExpr): opExprs) = createAst op expr (go opExpr opExprs)
        go e [] = e
    local NoAssoc = flip createAst <$> parseRest <*> parse <*> parseRest
    parseAssoc f = do
        rest <- parseRest
        onSplitted <- some ((,) <$> parse <*> parseRest)
        return $ f rest onSplitted
uberExpr [] elemParser createAst = elemParser
