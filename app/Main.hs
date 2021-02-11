module Main where

import Parser

import Data.Maybe (fromMaybe)
import Options.Applicative
import System.FilePath ((<.>))
import Text.Printf (printf)

data Input = FileInput FilePath
           | StrInput String
           deriving (Show)

data Output = FileOutput FilePath
            | DefaultOutput
            deriving (Show)

data Action = Action { input :: Input, output :: Output, dumpToStdout :: Bool, parserType :: ParserType }
            deriving (Show)

inputParser :: Parser Input
inputParser = fileInput <|> strInput

fileInput :: Parser Input
fileInput = FileInput <$> strOption
  (  short 'i'
  <> metavar "INPUT"
  <> help "Input file" )

strInput :: Parser Input
strInput = StrInput <$> argument str (metavar "STRING" <> help "String to parse")


outputParser :: Parser Output
outputParser = fileOutput <|> defaultOutput

fileOutput :: Parser Output
fileOutput = FileOutput <$> strOption
  (  short 'o'
  <> metavar "OUTPUT"
  <> help (printf "Output file. If not specified, output is INPUT.out. If INPUT is not specified, output is %s" defaultOutFile)
  )

defaultOutput :: Parser Output
defaultOutput = pure DefaultOutput

dumpToStdoutParer :: Parser Bool
dumpToStdoutParer = flag False True
  (  short 'd'
  <> help "Render input and output into stdout"
  )

parserTypeParser :: Parser ParserType
parserTypeParser = flag Infix Prefix
  (  short 'p'
  <> help "Use the parser for prefix binops. If not specified, use the parser for infix binops"
  )

actionParser :: Parser Action
actionParser =
  Action <$> inputParser <*> outputParser <*> dumpToStdoutParer <*> parserTypeParser

main :: IO ()
main = do
  runAction =<< execParser opts
  where
    opts = info (actionParser <**> helper)
      (  fullDesc
      <> progDesc "Sample parser for arithmetics"
      <> header "Sample parser"
      )

getInput :: Input -> IO String
getInput (FileInput path) = readFile path
getInput (StrInput str) = return str

getOutput :: Output -> Input -> IO FilePath
getOutput (FileOutput path) _ = return path
getOutput _ (FileInput path) = return $ path <.> "out"
getOutput _ _ = return defaultOutFile

defaultOutFile = "str.out"

runParser :: ParserType -> String -> FilePath -> IO ()
runParser pType s path =
  case parse pType s of
    Just expr ->
      writeFile path (printf "Expr:\n%s\n\nValue:\n%d\n" (show expr) (eval expr))
    Nothing ->
      writeFile path "Syntax error"

dumpIntoStdout :: Bool -> String -> FilePath -> IO ()
dumpIntoStdout False _ _ = return ()
dumpIntoStdout True i o = do
  out <- readFile o
  putStrLn $ printf "===================================\nInput:\n\n%s\n-----------------------------------\nOutput:\n\n%s\n===================================\n" i out

runAction :: Action -> IO ()
runAction (Action input output dump pType) = do
  i <- getInput input
  o <- getOutput output input
  runParser pType i o
  dumpIntoStdout dump i o