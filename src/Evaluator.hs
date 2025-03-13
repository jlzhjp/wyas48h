{-# LANGUAGE ExistentialQuantification #-}

module Evaluator (eval, Env, nullEnv, runIOThrows, liftThrows) where

import Common (LispError (..), LispVal (..), ThrowsError)
import Control.Monad.Except (ExceptT (..), MonadIO (liftIO), catchError, runExcept, runExceptT, throwError)
import Data.Functor ((<&>))
import Data.IORef (IORef, newIORef, writeIORef)
import Data.Maybe (isJust)
import GHC.IORef (readIORef)

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval env (Atom varId) = getVar env varId
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", ifPred, conseq, alt]) = do
  result <- eval env ifPred
  case result of
    Bool False -> eval env alt
    _ -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = case lookup func primitives of
  Just primFunc -> primFunc args
  Nothing -> throwError $ NotFunction "Unrecognized primitive function" func

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op params = Number . foldl1 op <$> mapM unpackNum params

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ head args
      right <- unpacker $ args !! 1
      return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
-- if the list is a [DottedList], then it should stay a [DottedList],
-- taking into account the improper tail
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
-- if you cons together two non-lists, or put a list in front, you get a [DottedList]
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
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

data Unpacker = forall a. (Eq a) => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  catchError
    ((==) <$> unpacker arg1 <*> unpacker arg2)
    (const $ return False) -- const: ignore the second argument

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2) unpackers
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool (primitiveEquals || eqvEquals == Bool True)
  where
    unpackers = [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
equal badArgList = throwError $ NumArgs 2 badArgList

type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ExceptT LispError IO

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows = ExceptT . return . runExcept

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = do
  result <- runExceptT action
  return $ either show id result

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef <&> (isJust . lookup var)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  -- lift the readIORef action into the combined monad
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . flip writeIORef value)
    (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings =
  readIORef envRef
    >>= extendEnv bindings
    >>= newIORef
  where
    extendEnv existingBindings env = fmap (++ env) (mapM addBinding existingBindings)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)