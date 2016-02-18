#!/usr/bin/env runhaskell
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment (getArgs)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
