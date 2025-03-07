module Main (main) where

import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Basic Arithmetic Tests" $ do
    it "Correctly adds two numbers" $ do
      (2 + 2 :: Integer) `shouldBe` 4

    it "Correctly multiplies two numbers" $ do
      (2 * 3 :: Integer) `shouldBe` 6

  describe "String Tests" $ do
    it "Correctly concatenates strings" $ do
      "Hello " ++ "World" `shouldBe` "Hello World"
