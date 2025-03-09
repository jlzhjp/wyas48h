module Main (main) where

import CommonSpec (commonSpec)
import EvaluatorSpec (evaluatorSpec)
import ParserSpec (parserSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  commonSpec
  parserSpec
  evaluatorSpec
