module Parser where

import AstStmt
import qualified Parser.Infix

parse :: String -> Either String Expr
parse = Parser.Infix.parse

