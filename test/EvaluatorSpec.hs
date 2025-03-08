module EvaluatorSpec(evaluatorSpec) where

import Lib (LispVal (..), doParse, eval)
import Test.Hspec
  ( Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
  )

evaluatorSpec :: Spec
evaluatorSpec = do
  describe "Mathematical operations" $ do
    describe "Addition" $ do
      it "adds two numbers" $ do
        case doParse "(+ 1 2)" of
          Right expr -> eval expr `shouldBe` Number 3
          Left err -> expectationFailure $ show err

      it "adds multiple numbers" $ do
        case doParse "(+ 1 2 3)" of
          Right expr -> eval expr `shouldBe` Number 6
          Left err -> expectationFailure $ show err

      it "handles negative numbers" $ do
        case doParse "(+ -1 5)" of
          Right expr -> eval expr `shouldBe` Number 4
          Left err -> expectationFailure $ show err

    describe "Subtraction" $ do
      it "subtracts two numbers" $ do
        case doParse "(- 5 3)" of
          Right expr -> eval expr `shouldBe` Number 2
          Left err -> expectationFailure $ show err

      it "subtracts multiple numbers" $ do
        case doParse "(- 10 2 3)" of
          Right expr -> eval expr `shouldBe` Number 5
          Left err -> expectationFailure $ show err

      it "handles negative numbers" $ do
        case doParse "(- 5 8)" of
          Right expr -> eval expr `shouldBe` Number (-3)
          Left err -> expectationFailure $ show err

    describe "Multiplication" $ do
      it "multiplies two numbers" $ do
        case doParse "(* 2 3)" of
          Right expr -> eval expr `shouldBe` Number 6
          Left err -> expectationFailure $ show err

      it "multiplies multiple numbers" $ do
        case doParse "(* 2 3 4)" of
          Right expr -> eval expr `shouldBe` Number 24
          Left err -> expectationFailure $ show err

      it "handles negative numbers" $ do
        case doParse "(* -2 3)" of
          Right expr -> eval expr `shouldBe` Number (-6)
          Left err -> expectationFailure $ show err

    describe "Division" $ do
      it "divides two numbers" $ do
        case doParse "(/ 6 3)" of
          Right expr -> eval expr `shouldBe` Number 2
          Left err -> expectationFailure $ show err

      it "performs integer division" $ do
        case doParse "(/ 7 2)" of
          Right expr -> eval expr `shouldBe` Number 3
          Left err -> expectationFailure $ show err

    describe "Modulo" $ do
      it "calculates modulo" $ do
        case doParse "(mod 7 3)" of
          Right expr -> eval expr `shouldBe` Number 1
          Left err -> expectationFailure $ show err

    describe "Quotient" $ do
      it "calculates quotient" $ do
        case doParse "(quotient 7 3)" of
          Right expr -> eval expr `shouldBe` Number 2
          Left err -> expectationFailure $ show err

    describe "Remainder" $ do
      it "calculates remainder" $ do
        case doParse "(remainder 7 3)" of
          Right expr -> eval expr `shouldBe` Number 1
          Left err -> expectationFailure $ show err

      it "handles negative dividend" $ do
        case doParse "(remainder -7 3)" of
          Right expr -> eval expr `shouldBe` Number (-1)
          Left err -> expectationFailure $ show err
