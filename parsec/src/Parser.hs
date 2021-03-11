module Parser where

import Megaparsec.Example

data ParserType = Mega deriving (Show)

parse :: ParserType -> String -> Either String Program
parse Mega = Megaparsec.Example.parse

