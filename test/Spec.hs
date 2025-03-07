module Main (main) where

import Test.Hspec (hspec)
import ParserSpec (parserSpec)

main :: IO ()
main = hspec $ do
  parserSpec