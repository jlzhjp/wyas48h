{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

-- | Common types and functions for the Scheme interpreter
module Common
  ( LispVal (..),
    LispError (..),
    unwordsList,
  )
where

import Text.Parsec (ParseError)

-- | Represents a Scheme value
data LispVal
  = -- | Symbolic atom
    Atom String
  | -- | List of values
    List [LispVal]
  | -- | Dotted list (proper pair)
    DottedList [LispVal] LispVal
  | -- | Integer number
    Number Integer
  | -- | String value
    String String
  | -- | Character value
    Character Char
  | -- | Boolean value
    Bool Bool
  deriving (Eq)

instance Show LispVal where
  show :: LispVal -> String
  show = \case
    String contents -> "\"" ++ contents ++ "\""
    Atom name -> name
    Number contents -> show contents
    Bool True -> "#t"
    Bool False -> "#f"
    List contents -> "(" ++ unwordsList contents ++ ")"
    DottedList listHead listTail -> "(" ++ unwordsList listHead ++ " . " ++ show listTail ++ ")"
    Character c -> "#\\" ++ showCharacter c
      where
        showCharacter ' ' = "space"
        showCharacter '\n' = "newline"
        showCharacter '\t' = "tab"
        showCharacter ch = [ch]

-- | Convert a list of LispVals to a space-separated string
unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

-- | Represents errors that can occur during evaluation
data LispError
  = -- | Wrong number of arguments
    NumArgs Integer [LispVal]
  | -- | Type mismatch
    TypeMismatch String LispVal
  | -- | Parse error
    Parser ParseError
  | -- | Invalid special form
    BadSpecialForm String LispVal
  | -- | Not a function
    NotFunction String String
  | -- | Unbound variable
    UnboundVar String String
  deriving (Eq)

instance Show LispError where
  show :: LispError -> String
  show = \case
    UnboundVar message varname -> message ++ ": " ++ varname
    BadSpecialForm message form -> message ++ ": " ++ show form
    NotFunction message func -> message ++ ": " ++ func
    NumArgs expected found -> "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
    TypeMismatch expected found -> "Invalid type: expected " ++ expected ++ ", found " ++ show found
    Parser parseErr -> "Parse error at " ++ show parseErr
