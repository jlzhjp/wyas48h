module Evaluator (eval) where

import Common (LispError (..), LispVal (..))
import Control.Monad.Except (Except, throwError)

eval :: LispVal -> Except LispError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = apply func =<< mapM eval args
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> Except LispError LispVal
apply func args = case lookup func primitives of
  Just primFunc -> primFunc args
  Nothing -> throwError $ NotFunction "Unrecognized primitive function args" func

primitives :: [(String, [LispVal] -> Except LispError LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> Except LispError LispVal
numericBinop op params = Number . foldl1 op <$> mapM unpackNum params

unpackNum :: LispVal -> Except LispError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum
