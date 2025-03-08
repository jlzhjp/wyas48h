module Main (main) where

import Lib (LispVal(..), doParse, eval)
import System.Environment (getArgs)

readExpr :: String -> LispVal
readExpr input = case doParse input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

main :: IO ()
main = getArgs >>= (print . eval . readExpr . (!! 0))
