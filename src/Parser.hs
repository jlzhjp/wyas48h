module Parser (doParse, LispVal (..)) where

import Control.Applicative (asum, (<|>))
import Data.Functor ((<&>))
import Numeric (readBin, readHex, readOct)
import Text.Parsec (ParseError, anyChar, string, try, unexpected)
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
  | Character Char
  | Bool Bool
  deriving (Show, Eq)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

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
    escaped :: Parser Char
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
  return $ Atom atom

parseDottedList :: Parser LispVal
parseDottedList = do
  listHead <- endBy parseExpr spaces
  listTail <- char '.' >> spaces >> parseExpr
  return $ DottedList listHead listTail

parseHashPrefixedLiteral :: Parser LispVal
parseHashPrefixedLiteral =
  char '#'
    >> (char 't' >> return (Bool True))
      <|> (char 'f' >> return (Bool False))
      <|> (char 'x' >> parseNumber Hex)
      <|> (char 'b' >> parseNumber Bin)
      <|> (char 'o' >> parseNumber Oct)
      <|> (char 'd' >> parseNumber Dec)
      <|> (char '\\' >> parseCharLiteral)

parseCharLiteral :: Parser LispVal
parseCharLiteral = try parseCharName <|> parseChar
  where
    parseChar = anyChar <&> Character
    parseCharName = do
      charName <- asum $ map string ["space", "newline", "tab"]
      case charName of
        "space" -> return $ Character ' '
        "newline" -> return $ Character '\n'
        "tab" -> return $ Character '\t'
        _ -> unexpected "unknown character name"

data NumericalBase = Hex | Dec | Oct | Bin

-- exercise 2.1 rewrite parseNumber using: 1. do-notation; 2. >>=
-- exercise 2.4 support scheme standard for different bases
{-
A number may be written in binary, octal, decimal, or hexadecimal
by the use of a radix prefix. The radix prefixes are #b (binary),
#o (octal), #d (decimal), and #x (hexadecimal). With no radix prefix,
a number is assumed to be expressed in decimal.
-}
parseNumber :: NumericalBase -> Parser LispVal
parseNumber base = case base of
  Hex -> hexLiteral
  Dec -> decLiteral
  Oct -> octLiteral
  Bin -> binLiteral
  where
    decLiteral :: Parser LispVal
    decLiteral = do
      number <- many1 digit
      return $ Number $ read number

    hexLiteral :: Parser LispVal
    hexLiteral = do
      number <- many1 (digit <|> oneOf "abcdefABCDEF")
      return $ case readHex number of
        [(n, _)] -> Number n
        _ -> undefined

    octLiteral :: Parser LispVal
    octLiteral = do
      number <- many1 (oneOf "01234567")
      return $ case readOct number of
        [(n, _)] -> Number n
        _ -> undefined

    binLiteral :: Parser LispVal
    binLiteral = do
      number <- many1 (oneOf "01")
      return $ case readBin number of
        [(n, _)] -> Number n
        _ -> undefined

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
    <|> parseHashPrefixedLiteral
    <|> parseString
    <|> parseNumber Dec
    <|> parseQuoted
    <|> do
      _ <- char '('
      x <- try parseList <|> parseDottedList
      _ <- char ')'
      return x

doParse :: String -> Either ParseError LispVal
doParse = parse parseExpr "lisp"
