{-# LANGUAGE InstanceSigs #-}

module Common (LispVal (..), LispError (..), unwordsList) where

import Text.Parsec (ParseError)

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Character Char
  | Bool Bool
  deriving (Eq)

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList listHead listTail) = "(" ++ unwordsList listHead ++ " . " ++ showVal listTail ++ ")"
showVal (Character c) = "#\\" ++ [c]

instance Show LispVal where
  show :: LispVal -> String
  show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  deriving (Eq)

showError :: LispError -> String
showError (UnboundVar message varname) =
  message ++ ": " ++ varname
showError (BadSpecialForm message form) =
  message ++ ": " ++ show form
showError (NotFunction message func) =
  message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) =
  "Parse error at " ++ show parseErr

instance Show LispError where
  show :: LispError -> String
  show = showError
