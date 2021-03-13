{-# LANGUAGE FlexibleInstances #-}
module Parser.Combinators where

import Control.Applicative ( Alternative(..) )
import Text.Printf (printf)
import Data.Char (isSpace)

data Result input a = Success input a
                    | Failure String
                    deriving (Show, Eq)

newtype Parser input a = Parser { runParser :: input -> Result input a }

instance Functor (Parser input) where
  -- fmap :: (a -> b) -> Parser input a -> Parser input b
  fmap f p = Parser $ \input ->
    case runParser p input of
      Success i x -> Success i (f x)
      Failure err -> Failure err

instance Applicative (Parser input) where
  pure x = Parser $ \input -> Success input x

  -- <*> :: Parser input (a -> b) -> Parser input a -> Parser input b
  p <*> q = Parser $ \input ->
    case runParser p input of
      Success i f ->
        case runParser q i of
          Success i' x -> Success i' (f x)
          Failure err -> Failure err
      Failure err -> Failure err

instance Monad (Parser input) where
  -- return :: a -> Parser input a
  return x = Parser $ \input -> Success input x

  -- >>= :: Parser input a -> (a -> Parser input b) -> Parser input b
  p >>= f = Parser $ \input ->
    case runParser p input of
      Success i x -> runParser (f x) i
      Failure err -> Failure err

instance Alternative (Parser input) where
  -- empty :: Parser input a
  empty = Parser $ \_ -> Failure "Empty parser"

  -- <|> :: Parser input a -> Parser input a -> Parser input a
  p <|> q = Parser $ \input ->
    case runParser p input of
      Failure err -> runParser q input
      Success i x -> Success i x

instance Show a => MonadFail (Parser [a]) where
  -- fail :: String -> Parser [a] String
  fail s = Parser $ \input ->
    case input of
      [] -> Failure "Unexpected EOF"
      (h:t) -> Failure $ printf "Syntax error on %s\n\t%s" (show h) s

item :: Parser [a] a
item = Parser $ \input ->
  case input of
    [] -> Failure "Empty input"
    (h:t) -> Success t h

satisfy :: Show a => (a -> Bool) -> Parser [a] a
satisfy pred = do
  x <- item
  if pred x
  then return x
  else fail "Predicate failed"

sepBy :: Parser input sep -> Parser input elem -> Parser input [elem]
sepBy sep elem = sepBy1 sep elem <|> return []

sepBy1 :: Parser input sep -> Parser input elem -> Parser input [elem]
sepBy1 sep elem = do
  h <- elem
  t <- many (sep *> elem)
  return (h : t)

brackets :: Parser input lbr -> Parser input rbr -> Parser input a -> Parser input a
brackets lbr rbr p = lbr *> p <* rbr

whitespaces :: Parser String ()
whitespaces =
  many (satisfy isSpace) *> return ()

eof :: (Show a) => Parser [a] ()
eof = Parser $ \input ->
  case input of
    [] -> Success [] ()
    (h:_) -> Failure $ printf "Syntax error on %s. Expected EOF" (show h)

parseMaybe :: Parser input a -> input -> Maybe a
parseMaybe p i =
  case runParser p i of
    Success _ x -> Just x
    _ -> Nothing

parseEither :: Parser input a -> input -> Either String a
parseEither p i =
  case runParser p i of
    Success _ x -> Right x
    Failure err -> Left err

symbol :: Char -> Parser String Char
symbol c = satisfy (==c)

word :: String -> Parser String String
word [] = return []
word (h:t) = do
  x <- symbol h
  y <- word t
  return (x:y)