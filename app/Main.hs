module Main (main) where

import Lib (eval, doParse)
import Control.Monad.Except (runExcept)
import System.Environment (getArgs)

main :: IO ()
main = do
  input <- parseArgs <$> getArgs
  putStrLn $ formatResult $ runExcept (doParse input >>= eval)
  where
    parseArgs args = if null args then "()" else head args
    formatResult = either (\err -> "Error: " ++ show err) show
