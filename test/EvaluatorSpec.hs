{-# LANGUAGE LambdaCase #-}
module EvaluatorSpec (evaluatorSpec) where

import Control.Monad.Except (runExcept)
import Lib (LispVal (..), LispError(..), doParse, eval)
import Test.Hspec
  ( Expectation,
    Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
    shouldSatisfy,
  )

-- Helper function for evaluating expressions in tests
shouldEvalTo :: String -> LispVal -> Expectation
shouldEvalTo exprStr expected = do
  case runExcept (doParse exprStr >>= eval) of
    Right result -> result `shouldBe` expected
    Left err -> expectationFailure $ show err

-- Helper function for testing error conditions with predicates
shouldThrowError :: String -> (LispError -> Bool) -> Expectation
shouldThrowError exprStr errorPredicate = do
  case runExcept (doParse exprStr >>= eval) of
    Right result -> expectationFailure $ "Expected error but got result: " ++ show result
    Left err -> err `shouldSatisfy` errorPredicate

-- Better helper function for testing specific expected errors
shouldFailWith :: String -> LispError -> Expectation
shouldFailWith exprStr expectedError = do
  case runExcept (doParse exprStr >>= eval) of
    Right result -> expectationFailure $ "Expected error " ++ show expectedError ++ " but got result: " ++ show result
    Left err -> err `shouldBe` expectedError

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

  describe "Error conditions" $ do
    describe "Unrecognized special form" $ do
      it "throws error for unknown form" $
        "(unknown-form 1 2)" `shouldFailWith` NotFunction "Unrecognized primitive function args" "unknown-form"

    describe "Numeric operations errors" $ do
      it "throws error when adding non-numbers" $
        "(+ 1 \"hello\")" `shouldFailWith` TypeMismatch "number" (String "hello")

      it "throws error when subtracting non-numbers" $
        "(- 1 #t)" `shouldFailWith` TypeMismatch "number" (Bool True)

    describe "Boolean operations errors" $ do
      it "throws error when comparing wrong number of arguments" $
        "(< 1 2 3)" `shouldFailWith` NumArgs 2 [Number 1, Number 2, Number 3]

      it "throws error when comparing non-numbers" $
        "(< 1 \"hello\")" `shouldFailWith` TypeMismatch "number" (String "hello")

    describe "List operations errors" $ do
      it "throws error when car gets non-list" $
        "(car 1)" `shouldFailWith` TypeMismatch "pair" (Number 1)

      it "throws error when car gets wrong number of arguments" $
        "(car)" `shouldFailWith` NumArgs 1 []

      it "throws error when cdr gets non-list" $
        "(cdr \"hello\")" `shouldFailWith` TypeMismatch "pair" (String "hello")

      -- This test uses the old style for cases where the exact error structure might be complex
      it "throws error when cdr gets wrong number of arguments" $
        "(cdr 1 2)" `shouldThrowError` \case
          NumArgs 1 _ -> True
          _ -> False
