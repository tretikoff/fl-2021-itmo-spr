module Parser where

import Expr (Expr (..))
import qualified Parser.Prefix
import qualified Parser.Infix

data ParserType = Prefix | Infix deriving (Show)

parse :: ParserType -> String -> Either String Expr
parse Prefix = Parser.Prefix.parse
parse Infix  = Parser.Infix.parse

