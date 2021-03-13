module Test.Parser.Common where

import           Control.Applicative ((<|>))
import           AstStmt             (Operator (..), Expr (..))
import Expr (toOp)
import           Test.Tasty.HUnit    (Assertion (..), (@?=))
import           Parser.Common       (Associativity (..), uberExpr, number)
import           Parser.Combinators  (Parser (..), Result (..), word, symbol, parseEither)
import           Test.Common         (syntaxError)

mult  = toOp <$> word "*"
sum'  = toOp <$> word "+"
minus = toOp <$> word "-"
div'  = toOp <$> word "/"

expr1 :: Parser String Expr
expr1 =
  uberExpr [ (mult, LeftAssoc)
           , (minus <|> div', RightAssoc)
           , (sum', NoAssoc)
           ]
           (Num <$> number <|> symbol '(' *> expr1 <* symbol ')')
           BinOp

expr2 :: Parser String Expr
expr2 =
  uberExpr [(mult <|> div' <|> minus <|> sum', LeftAssoc)]
           (Num <$> number)
           BinOp

unit_expr1 :: Assertion
unit_expr1 = do
  runParser expr1 "13" @?= Success "" (Num 13)
  runParser expr1 "(((1)))" @?= Success "" (Num 1)
  runParser expr1 "1+2*3-4/5" @?= Success "" (BinOp Mult (BinOp Plus (Num 1) (Num 2)) (BinOp Minus (Num 3) (BinOp Div (Num 4) (Num 5))))
  runParser expr1 "1+2+3" @?= Success "+3" (BinOp Plus (Num 1) (Num 2))
  runParser expr1 "1*2*3" @?= Success "" (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))
  runParser expr1 "1/2/3" @?= Success "" (BinOp Div (Num 1) (BinOp Div (Num 2) (Num 3)))
  runParser expr1 "1-2-3" @?= Success "" (BinOp Minus (Num 1) (BinOp Minus (Num 2) (Num 3)))
  runParser expr1 "1-2*3/4+5*6-7-8/9" @?= Success "" (BinOp Mult (BinOp Mult (BinOp Minus (Num 1) (Num 2)) (BinOp Div (Num 3) (BinOp Plus (Num 4) (Num 5)))) (BinOp Minus (Num 6) (BinOp Minus (Num 7) (BinOp Div (Num 8) (Num 9)))))

unit_expr2 :: Assertion
unit_expr2 = do
  runParser expr2 "13" @?= Success "" (Num 13)
  runParser expr2 "1+2*3-4/5" @?= Success "" (BinOp Div (BinOp Minus (BinOp Mult (BinOp Plus (Num 1) (Num 2)) (Num 3)) (Num 4)) (Num 5))
  runParser expr2 "1+2+3" @?= Success "" (BinOp Plus (BinOp Plus (Num 1) (Num 2)) (Num 3))
  runParser expr2 "1*2*3" @?= Success "" (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))
  runParser expr2 "1/2/3" @?= Success "" (BinOp Div (BinOp Div (Num 1) (Num 2)) (Num 3))
  runParser expr2 "1-2-3" @?= Success "" (BinOp Minus (BinOp Minus (Num 1) (Num 2)) (Num 3))
  runParser expr2 "1-2*3/4+5*6-7-8/9" @?= Success "" (BinOp Div (BinOp Minus (BinOp Minus (BinOp Mult (BinOp Plus (BinOp Div (BinOp Mult (BinOp Minus (Num 1) (Num 2)) (Num 3)) (Num 4)) (Num 5)) (Num 6)) (Num 7)) (Num 8)) (Num 9))
  syntaxError (parseEither expr2) "(((1)))"
