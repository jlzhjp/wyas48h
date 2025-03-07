module Main (main) where

import Lib (doParse)
import System.Environment (getArgs)

readExpr :: String -> [Char]
readExpr input = case doParse input of
  Left err -> "No match: " ++ show err
  Right _ -> "Found value"

main :: IO ()
main =
  do
    args <- getArgs
    putStrLn $ readExpr $ head args
