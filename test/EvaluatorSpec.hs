module EvaluatorSpec (evaluatorSpec) where

import Control.Monad.Except (runExcept)
import Lib (LispVal (..), doParse, eval)
import Test.Hspec
  ( Expectation,
    Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
  )

-- Helper function for evaluating expressions in tests
shouldEvalTo :: String -> LispVal -> Expectation
shouldEvalTo exprStr expected = do
  case runExcept (doParse exprStr >>= eval) of
    Right result -> result `shouldBe` expected
    Left err -> expectationFailure $ show err

evaluatorSpec :: Spec
evaluatorSpec = do
  describe "Mathematical operations" $ do
    describe "Addition" $ do
      it "adds two numbers" $
        "(+ 1 2)" `shouldEvalTo` Number 3

      it "adds multiple numbers" $
        "(+ 1 2 3)" `shouldEvalTo` Number 6

      it "handles negative numbers" $
        "(+ -1 5)" `shouldEvalTo` Number 4

    describe "Subtraction" $ do
      it "subtracts two numbers" $
        "(- 5 3)" `shouldEvalTo` Number 2

      it "subtracts multiple numbers" $
        "(- 10 2 3)" `shouldEvalTo` Number 5

      it "handles negative numbers" $
        "(- 5 8)" `shouldEvalTo` Number (-3)

    describe "Multiplication" $ do
      it "multiplies two numbers" $
        "(* 2 3)" `shouldEvalTo` Number 6

      it "multiplies multiple numbers" $
        "(* 2 3 4)" `shouldEvalTo` Number 24

      it "handles negative numbers" $
        "(* -2 3)" `shouldEvalTo` Number (-6)

    describe "Division" $ do
      it "divides two numbers" $
        "(/ 6 3)" `shouldEvalTo` Number 2

      it "performs integer division" $
        "(/ 7 2)" `shouldEvalTo` Number 3

    describe "Modulo" $ do
      it "calculates modulo" $
        "(mod 7 3)" `shouldEvalTo` Number 1

    describe "Quotient" $ do
      it "calculates quotient" $
        "(quotient 7 3)" `shouldEvalTo` Number 2

    describe "Remainder" $ do
      it "calculates remainder" $
        "(remainder 7 3)" `shouldEvalTo` Number 1

      it "handles negative dividend" $
        "(remainder -7 3)" `shouldEvalTo` Number (-1)
