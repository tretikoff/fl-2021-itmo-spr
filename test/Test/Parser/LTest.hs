module Test.Parser.LTest where
import Parser.Combinators
import Test.Tasty.HUnit (Assertion, (@?=))

import Test.Common (syntaxError, parsingSuccess)
import Data.List
import AstStmt
import LParser

keywords = ["if", "while", "var", "read", "write", ";"]
code0 = ["var x = 5;"]
code1 = ["var x = 5+10;"]
code2 = ["var x = 5;", "if (x > 3) then { write(x); } else {write(x +  5); }"]
code3 = ["var x = 5;", "while (x < 10)","{", "read(x);", "}", "write(x)"]

parseSq = parseEither (whitespaces *> parseSeq <* whitespaces <* eof)
parseSt = parseEither (whitespaces *> parseStatement <* whitespaces <* eof)

whileP = "while (5 + 10) { var xyz = 50; }"
whileSt = (While (BinOp Plus (Num 5) (Num 10)) (Assign "xyz" (Num 50)))
readP = "read(xyz );"
readSt = (Read "xyz")
writeP = "write( 500 + 25);"
writeSt = (Write (BinOp Plus (Num 500) (Num 25)))
seqP = intercalate " " [readP, writeP, whileP]
unit_success:: Assertion
unit_success = do
  parsingSuccess parseSt "var x = 5;" (Assign "x" (Num 5))
  parsingSuccess parseSt "var _x5 = 5;" (Assign "_x5" (Num 5))
  parsingSuccess parseSt "var someVar = ( 555+5);" (Assign "someVar" $ BinOp Plus (Num 555) (Num 5))
  parsingSuccess parseSt readP (Read "xyz")
  parsingSuccess parseSt writeP (Write (BinOp Plus (Num 500) (Num 25)))
  parsingSuccess parseSq seqP $ Seq [readSt, writeSt, whileSt]

unit_failure = do
  syntaxError parseSt "var 5_x = 5"
  syntaxError parseSt "var 0x = 5"

unit_conditionals:: Assertion
unit_conditionals = do
  parsingSuccess parseSt whileP whileSt
  parsingSuccess parseSt ("if (55) { " ++ readP ++ " }") (If (Num 55) readSt Nothing)
  parsingSuccess parseSt ("if (35) {" ++ readP ++ " } else { " ++ writeP ++ "}") (If (Num 35) readSt (Just writeSt))

program2 = Seq [Assign "x" (Num 5), If (BinOp Gt (Ident "x") (Num 3)) (Write $ Ident "x") (Just $ Write $ BinOp Plus (Ident "x") (Num 5))]

unit_equivalence:: Assertion
unit_equivalence = do
  parseSq (printer $ Program program2) @?= Right program2
