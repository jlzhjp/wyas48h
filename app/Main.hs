module Main (main) where

import Lib (eval, doParse)
import Control.Monad.Except (runExcept)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let input = if null args then "()" else head args
  let result = runExcept (doParse input >>= eval)
  putStrLn $ case result of
    Left err -> "Error: " ++ show err
    Right val -> show val
