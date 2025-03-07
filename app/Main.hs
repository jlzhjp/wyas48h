module Main (main) where

import Lib (readExpr)
import System.Environment (getArgs)

main :: IO ()
main =
  do
    args <- getArgs
    putStrLn $ readExpr $ head args
