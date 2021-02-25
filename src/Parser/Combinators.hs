{-# LANGUAGE FlexibleInstances #-}
module Parser.Combinators where

import Control.Applicative
import Text.Printf (printf)
import Data.Char (isDigit, isAlpha)

data Result input a = Success input a
                    | Failure String

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

digit :: Parser String Char
digit = satisfy isDigit

digit' :: Parser String Char
digit' = satisfy (\x -> isDigit x && x /= '0')

alpha :: Parser String Char
alpha = satisfy isAlpha

-- number :: not empty, digits
number :: Parser String Int
number = do
  num <- some digit
  return $ (read num :: Int)

sepBy :: Parser input sep -> Parser input elem -> Parser input [elem]
sepBy sep elem = sepBy1 sep elem <|> return []

sepBy1 :: Parser input sep -> Parser input elem -> Parser input [elem]
sepBy1 sep elem = do
  h <- elem
  t <- many (sep *> elem)
  return (h : t)


brackets :: Parser input lbr -> Parser input rbr -> Parser input a -> Parser input a
brackets lbr rbr p = lbr *> p <* rbr
-- = do
--   lbr
--   x <- p
--   rbr
--   return x