module Megaparsec.Example where


import Control.Monad.Combinators.Expr
    ( makeExprParser, Operator(InfixR) )
import Data.Void (Void(..))
import Text.Megaparsec
    ( (<|>),
      runParser,
      errorBundlePretty,
      between,
      many,
      Parsec,
      MonadParsec(try) )
import Text.Megaparsec.Char
    ( alphaNumChar, char, letterChar, lowerChar, space1, upperChar )
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parse :: String -> Either String Program
parse = runBundlingParser progParser

runBundlingParser parser =
    mapLeft errorBundlePretty . runParser parser ""
  where
    mapLeft f = either (Left . f) Right

-- spaces & comments
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

kw :: [String]
kw = ["data"]

identLower :: Parser String
identLower = identParser lowerChar

identUpper :: Parser String
identUpper = identParser upperChar

ident :: Parser String
ident = identParser letterChar

identParser fstLetter = (lexeme . try) (p >>= check)
  where
    p = (:) <$> fstLetter <*> many (char '_' <|> alphaNumChar)
    check x = if x `elem` kw
              then fail $ show x ++ " cannot be an identifier"
              else return x

-- brackets
roundBr :: Parser a -> Parser a
roundBr = between (symbol "(") (symbol ")")

maybeBr :: Parser a -> Parser a
maybeBr p = roundBr (maybeBr p) <|> p

data Program = Program [Statement]
             deriving (Show, Eq)

data Statement = Decl Name [Param] [Constr]
               | FunType Name Type
               deriving (Show, Eq)

data Type = Base Constr | Arrow Type Type
          deriving (Show, Eq)

type Name = String  -- upper
type Param = String -- lower

data Constr = Constr Name [Constr]
            | Param Param
            deriving (Show, Eq)

progParser :: Parser Program
progParser = Program <$>
  many ((declParser <|> funTypeParser) <* symbol ";")

funTypeParser :: Parser Statement
funTypeParser = do
  name <- identLower
  symbol "::"
  typ <- typeParser
  return $ FunType name typ

typeParser :: Parser Type
typeParser =
    makeExprParser
      (roundBr typeParser <|> Base <$> constrParser)
      operatorTable
  where
    operatorTable =
      [ [rightAssoc "->" Arrow] ]
    rightAssoc name f = InfixR (f <$ symbol name)

constrParser :: Parser Constr
constrParser = maybeBr $
  Param <$> identLower <|>
  Constr <$> identUpper <*> many constrParser

declParser :: Parser Statement
declParser = do
  symbol "data"
  name <- identUpper
  params <- many identLower
  symbol "="
  h <- constrParser
  t <- many (symbol "|" *> constrParser)
  return $ Decl name params (h:t)



























