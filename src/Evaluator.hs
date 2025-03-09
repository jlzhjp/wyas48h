{-# LANGUAGE ExistentialQuantification #-}

module Evaluator (eval) where

import Common (LispError (..), LispVal (..))
import Control.Monad.Except (Except, catchError, throwError)

eval :: LispVal -> Except LispError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", ifPred, conseq, alt]) = do
  result <- eval ifPred
  case result of
    Bool False -> eval alt
    _ -> eval conseq
eval (List (Atom func : args)) = apply func =<< mapM eval args
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> Except LispError LispVal
apply func args = case lookup func primitives of
  Just primFunc -> primFunc args
  Nothing -> throwError $ NotFunction "Unrecognized primitive function" func

primitives :: [(String, [LispVal] -> Except LispError LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string>?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<)),
    ("string>=?", strBoolBinop (>=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eqv?", eqv),
    ("equal?", equal)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> Except LispError LispVal
numericBinop op params = Number . foldl1 op <$> mapM unpackNum params

boolBinop :: (LispVal -> Except LispError a) -> (a -> a -> Bool) -> [LispVal] -> Except LispError LispVal
boolBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ head args
      right <- unpacker $ args !! 1
      return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> Except LispError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> Except LispError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> Except LispError LispVal
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> Except LispError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> Except LispError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> Except LispError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

car :: [LispVal] -> Except LispError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> Except LispError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> Except LispError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
-- if the list is a [DottedList], then it should stay a [DottedList],
-- taking into account the improper tail
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
-- if you cons together two non-lists, or put a list in front, you get a [DottedList]
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> Except LispError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2]
  | length arg1 /= length arg2 = return $ Bool False
  | otherwise = do
      results <- mapM (\(x, y) -> eqv [x, y]) (zip arg1 arg2)
      return $ Bool $ all isBoolTrue results
  where
    isBoolTrue (Bool True) = True
    isBoolTrue _ = False
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. (Eq a) => AnyUnpacker (LispVal -> Except LispError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> Except LispError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  catchError
    ((==) <$> unpacker arg1 <*> unpacker arg2)
    (const $ return False) -- const: ignore the second argument

equal :: [LispVal] -> Except LispError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2) unpackers
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool (primitiveEquals || eqvEquals == Bool True)
  where
    unpackers = [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
equal badArgList = throwError $ NumArgs 2 badArgList