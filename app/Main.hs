module Main (main) where

import Control.Monad.Except (runExcept)
import Lib (doParse, eval)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ either (\x -> "Error: " ++ show x) show $ runExcept (doParse expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: (Monad m) => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  result <- prompt
  if predicate result
    then return ()
    else action result >> until_ predicate prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> evalAndPrint $ head args
    _ -> putStrLn "Program takes only 0 or 1 argument"
