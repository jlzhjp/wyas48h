module Parser (doParse, LispVal (..)) where

import Control.Applicative ((<|>))
import Text.Parsec (ParseError, try, anyChar, unexpected)
import Text.ParserCombinators.Parsec
  ( Parser,
    char,
    digit,
    endBy,
    letter,
    many,
    many1,
    noneOf,
    oneOf,
    parse,
    sepBy,
    skipMany1,
    space,
  )

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  deriving (Show, Eq)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

-- exercise 2.2, 2.3: handle escaped characters in strings
parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (escaped <|> noneOf "\"")
  _ <- char '"'
  return $ String x
  where
    escaped:: Parser Char
    escaped = do
      _ <- char '\\'
      c <- anyChar
      case c of
        '"' -> return '"'
        't' -> return '\t'
        'n' -> return '\n'
        'r' -> return '\r'
        '\\' -> return '\\'
        _ -> unexpected $ "escape sequence \\" ++ [c]

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseDottedList :: Parser LispVal
parseDottedList = do
  listHead <- endBy parseExpr spaces
  listTail <- char '.' >> spaces >> parseExpr
  return $ DottedList listHead listTail

-- exercise 2.1 rewrite parseNumber using: 1. do-notation; 2. >>=
parseNumber :: Parser LispVal
parseNumber = do
  digitStr <- many1 digit
  return $ Number . read $ digitStr

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do
      _ <- char '('
      x <- try parseList <|> parseDottedList
      _ <- char ')'
      return x

doParse :: String -> Either ParseError LispVal
doParse = parse parseExpr "lisp"