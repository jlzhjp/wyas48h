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

  describe "Special forms" $ do
    describe "if expressions" $ do
      it "evaluates true condition" $
        "(if #t 1 2)" `shouldEvalTo` Number 1
        
      it "evaluates false condition" $
        "(if #f 1 2)" `shouldEvalTo` Number 2
        
      it "evaluates condition expression" $
        "(if (> 2 1) \"yes\" \"no\")" `shouldEvalTo` String "yes"
        
      it "evaluates nested if expressions - true branch" $
        "(if #t (if #t 1 2) 3)" `shouldEvalTo` Number 1
        
      it "evaluates nested if expressions - false branch" $
        "(if #f 1 (if #t 2 3))" `shouldEvalTo` Number 2
      
      it "throws error with wrong number of arguments" $
        "(if #t)" `shouldFailWith` BadSpecialForm "Unrecognized special form" (List [Atom "if", Bool True])

  describe "Error conditions" $ do
    describe "Unrecognized special form" $ do
      it "throws error for unknown form" $
        "(unknown-form 1 2)" `shouldFailWith` NotFunction "Unrecognized primitive function" "unknown-form"

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

  describe "List operations" $ do
    describe "car" $ do
      it "gets the first element of a list" $
        "(car '(1 2 3))" `shouldEvalTo` Number 1
        
      it "gets the first element of a dotted list" $
        "(car '(1 2 . 3))" `shouldEvalTo` Number 1
        
      it "works with nested lists" $
        "(car '((1 2) 3 4))" `shouldEvalTo` List [Number 1, Number 2]
        
    describe "cdr" $ do
      it "gets the rest of a list" $
        "(cdr '(1 2 3))" `shouldEvalTo` List [Number 2, Number 3]
        
      it "gets the rest of a dotted list" $
        "(cdr '(1 2 . 3))" `shouldEvalTo` DottedList [Number 2] (Number 3)
        
      it "returns empty list for singleton list" $
        "(cdr '(1))" `shouldEvalTo` List []
        
      it "returns the tail for singleton dotted list" $
        "(cdr '(1 . 2))" `shouldEvalTo` Number 2
        
    describe "cons" $ do
      it "adds an element to the beginning of a list" $
        "(cons 1 '(2 3))" `shouldEvalTo` List [Number 1, Number 2, Number 3]
        
      it "creates a new singleton list when given empty list" $
        "(cons 1 '())" `shouldEvalTo` List [Number 1]
        
      it "creates a dotted list with non-list second argument" $
        "(cons 1 2)" `shouldEvalTo` DottedList [Number 1] (Number 2)
        
      it "preserves dotted list structure" $
        "(cons 1 '(2 . 3))" `shouldEvalTo` DottedList [Number 1, Number 2] (Number 3)
        
      it "throws error with wrong number of arguments" $
        "(cons 1)" `shouldFailWith` NumArgs 2 [Number 1]
        
    describe "eqv?" $ do
      it "compares equal numbers" $
        "(eqv? 1 1)" `shouldEvalTo` Bool True
        
      it "compares unequal numbers" $
        "(eqv? 1 2)" `shouldEvalTo` Bool False
        
      it "compares equal booleans" $
        "(eqv? #t #t)" `shouldEvalTo` Bool True
        
      it "compares unequal booleans" $
        "(eqv? #t #f)" `shouldEvalTo` Bool False
        
      it "compares equal strings" $
        "(eqv? \"hello\" \"hello\")" `shouldEvalTo` Bool True
        
      it "compares unequal strings" $
        "(eqv? \"hello\" \"world\")" `shouldEvalTo` Bool False
        
      it "compares equal atoms" $
        "(eqv? 'abc 'abc)" `shouldEvalTo` Bool True
        
      it "compares equal lists" $
        "(eqv? '(1 2 3) '(1 2 3))" `shouldEvalTo` Bool True
        
      it "compares unequal lists" $
        "(eqv? '(1 2) '(1 3))" `shouldEvalTo` Bool False
        
      it "compares lists of different lengths" $
        "(eqv? '(1 2) '(1 2 3))" `shouldEvalTo` Bool False
        
      it "compares dotted lists" $
        "(eqv? '(1 2 . 3) '(1 2 . 3))" `shouldEvalTo` Bool True
        
      it "returns false for different types" $
        "(eqv? 1 \"1\")" `shouldEvalTo` Bool False
        
      it "throws error with wrong number of arguments" $
        "(eqv? 1 2 3)" `shouldFailWith` NumArgs 2 [Number 1, Number 2, Number 3]
