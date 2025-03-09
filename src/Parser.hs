{-# LANGUAGE LambdaCase #-}

module Parser (doParse, LispVal (..)) where

import Common (LispError (Parser), LispVal (..))
import Control.Applicative (asum, (<|>))
import Control.Monad.Except (Except, liftEither)
import Data.Bifunctor (first)
import Numeric (readBin, readHex, readOct)
import Text.Parsec (anyChar, between, string, try, unexpected)
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

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- exercise 2.2, 2.3: handle escaped characters in strings
parseString :: Parser LispVal
parseString = String <$> between (char '"') (char '"') (many (escaped <|> noneOf "\""))
  where
    escaped :: Parser Char
    escaped =
      char '\\' *> anyChar >>= \c -> case c of
        '"' -> return '"'
        't' -> return '\t'
        'n' -> return '\n'
        'r' -> return '\r'
        '\\' -> return '\\'
        _ -> unexpected $ "escape sequence \\" ++ [c]

parseAtom :: Parser LispVal
parseAtom = Atom <$> ((:) <$> (letter <|> symbol) <*> many (letter <|> digit <|> symbol))

parseDottedList :: Parser LispVal
parseDottedList = DottedList <$> endBy parseExpr spaces <*> (char '.' *> spaces *> parseExpr)

parseHashPrefixedLiteral :: Parser LispVal
parseHashPrefixedLiteral =
  char '#'
    *> asum
      [ Bool True <$ char 't',
        Bool False <$ char 'f',
        char 'x' *> parseNumber Hex,
        char 'b' *> parseNumber Bin,
        char 'o' *> parseNumber Oct,
        char 'd' *> parseNumber Dec,
        char '\\' *> parseCharLiteral
      ]

parseCharLiteral :: Parser LispVal
parseCharLiteral = try parseCharName <|> (Character <$> anyChar)
  where
    charMap = [("space", ' '), ("newline", '\n'), ("tab", '\t')]
    parseCharName = do
      name <- asum $ map (string . fst) charMap
      case lookup name charMap of
        Just c -> return $ Character c
        Nothing -> unexpected "unknown character name"

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
parseNumber = \case
  Hex -> readWith readHex (digit <|> oneOf "abcdefABCDEF")
  Dec -> Number . read <$> many1 digit
  Oct -> readWith readOct (oneOf "01234567")
  Bin -> readWith readBin (oneOf "01")
  where
    readWith reader chars = do
      number <- many1 chars
      case reader number of
        [(n, _)] -> return $ Number n
        _ -> unexpected "invalid number format"

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseQuoted :: Parser LispVal
parseQuoted = char '\'' *> (List . (Atom "quote" :) . return <$> parseExpr)

parseBackquoted :: Parser LispVal
parseBackquoted = char '`' *> (List . (Atom "quasiquote" :) . return <$> parseExpr)

parseUnquoted :: Parser LispVal
parseUnquoted = char ',' *> (List . (Atom "unquote" :) . return <$> parseExpr)

parseUnquotedSpliced :: Parser LispVal
parseUnquotedSpliced = string ",@" *> (List . (Atom "unquote-splicing" :) . return <$> parseExpr)

parseExpr :: Parser LispVal
parseExpr =
  asum
    [ parseAtom,
      parseHashPrefixedLiteral,
      parseString,
      parseNumber Dec,
      parseQuoted,
      try parseUnquotedSpliced,
      parseUnquoted,
      parseBackquoted,
      between (char '(') (char ')') (try parseList <|> parseDottedList)
    ]

doParse :: String -> Except LispError LispVal
doParse = liftEither . first Parser . parse parseExpr "lisp"