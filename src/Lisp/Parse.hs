module Lisp.Parse where

import Text.ParserCombinators.Parsec (Parser, (<|>))
import qualified Text.ParserCombinators.Parsec as P
import Lisp.Core
import Data.Bifunctor (Bifunctor(first))

readExpr :: String -> ThrowsError LispVal
readExpr = runReadExpr parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = runReadExpr (P.endBy parseExpr P.spaces)

runReadExpr :: P.Parser a -> String -> ThrowsError a
runReadExpr parser input = first Parser $ P.parse parser "lisp" input

parseExpr :: Parser LispVal
parseExpr =
  parseAtom <|> parseString <|> parseNumeric <|> parseQuoted <|> parseLists
  where
    parseNumeric = P.try parseFloat <|> parseNumber
    parseLists = do
      P.char '('
      x <- P.try parseList <|> parseDottedList
      P.char ')'
      return x

parseString :: Parser LispVal
parseString = do
  P.char '"'
  x <- P.many $ P.try quote <|> P.noneOf "\""
  P.char '"'
  pure $ String x

symbol :: Parser Char
symbol = P.oneOf "!#$%&|*+-/:<=>?@^_~"

quote :: Parser Char
quote = P.string "\\\"" >> pure '"'

parseAtom :: Parser LispVal
parseAtom = do
  first <- P.letter <|> symbol
  rest <- P.many (P.letter <|> P.digit <|> symbol)
  let atom = first : rest
  return $
    case atom of
      "#t" -> Bool True
      "#f" -> Bool False
      _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> P.many1 P.digit

parseFloat :: Parser LispVal
parseFloat =
  Float . read <$> do
    init <- P.many1 P.digit
    dot <- P.char '.'
    tail <- P.many1 P.digit
    pure $ init ++ dot : tail

parseList :: Parser LispVal
parseList = List <$> P.sepBy parseExpr P.spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- P.endBy parseExpr P.spaces
  tail <- P.char '.' >> P.spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  P.char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]
