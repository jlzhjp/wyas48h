module Main (main) where

import EvaluatorSpec (evaluatorSpec)
import ParserSpec (parserSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  parserSpec
  evaluatorSpec
