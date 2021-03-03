module Parser.Common where

import Data.Char (isDigit, isAlpha)
import Parser.Combinators ( Parser, satisfy )
import Control.Applicative (some)

digit :: Parser String Char
digit = satisfy isDigit

digit' :: Parser String Char
digit' = satisfy (\x -> isDigit x && x /= '0')

alpha :: Parser String Char
alpha = satisfy isAlpha

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
uberExpr x el_p createBinAst = local x where
    local ((p, ass) : xs) = case ass of
        LeftAssoc ->
        RightAssoc ->
        NoAssoc ->
    local [] = el_p
