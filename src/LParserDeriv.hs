module LParserDeriv where

import Data.Char
import AstStmt
import Control.Applicative ( Alternative((<|>)), many )
import Parser.Combinators
import Parser.Infix
import Parser.Common


parseSeparators = many (satisfy isSeparator)

parseKeyword keyword = whitespaces *> word keyword <* whitespaces

parseIdent = whitespaces *> ident <* whitespaces
parseAssign = Assign <$> (parseKeyword "var" *> parseIdent <* (symbol '=')) <*> (parseExpr)
parseIgnore = Ignore <$> parseExpr
parseSemicolonStatement = (parseRead <|>  parseWrite  <|> parseAssign <|> parseIgnore) <* symbol ';'
parseSimpleStatement = (parseSemicolonStatement <|> parseWhile <|> parseIf)
parseStatement = parseSimpleStatement <|> parseSeq

parseSpacedExpr = whitespaces *> parseExpr <* whitespaces
parseBracketsStatement = whitespaces *> brackets (symbol '{') (symbol '}') (whitespaces *> parseStatement <* whitespaces) <* whitespaces
parseWhile = While <$> (parseKeyword "while" *> parseSpacedExpr) <*> parseBracketsStatement
parseRead = AstStmt.Read <$> (parseKeyword "read" *> brackets opBr clBr parseIdent)
parseWrite = Write <$> (parseKeyword "write" *> brackets opBr clBr parseSpacedExpr)
parseSeq = Seq <$> (many parseSimpleStatement)
parseIf = let
    parseElse = Parser $ \input -> case runParser (parseKeyword "else" *> parseBracketsStatement) input of
        Success i x -> Success i $ Just x
        Failure err -> Success "" Nothing
    in do
        expr <- (parseKeyword "if" *> parseSpacedExpr)
        thn <- parseBracketsStatement
        els <- parseElse
        return $ If expr thn els

parseProgram = parseSeq