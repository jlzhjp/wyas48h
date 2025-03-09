module ParserSpec (parserSpec) where

import Control.Monad.Except (runExcept)
import Lib (LispVal (..), doParse)
import Test.Hspec
  ( Expectation,
    Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
    shouldContain,
  )

-- Helper function for parsing expressions in tests
shouldParseTo :: String -> LispVal -> Expectation
shouldParseTo input expected = 
  runExcept (doParse input) `shouldBe` Right expected

-- Helper function for tests expecting parse errors
shouldFailWithError :: String -> String -> Expectation
shouldFailWithError input errorSubstring = 
  case runExcept (doParse input) of
    Left err -> show err `shouldContain` errorSubstring
    Right val -> expectationFailure $ "expected parse to fail, got " ++ show val

parserSpec :: Spec
parserSpec = do
  describe "Parser" $ do
    describe "parseAtom" $ do
      it "parses a simple atom" $
        "abc" `shouldParseTo` Atom "abc"

      it "parses an atom with symbols" $
        "a-b_c!$%&" `shouldParseTo` Atom "a-b_c!$%&"

      it "parses boolean true" $
        "#t" `shouldParseTo` Bool True

      it "parses boolean false" $
        "#f" `shouldParseTo` Bool False

    describe "parseString" $ do
      it "parses a simple string" $
        "\"hello\"" `shouldParseTo` String "hello"

      it "parses an empty string" $
        "\"\"" `shouldParseTo` String ""

      it "parses a string with spaces" $
        "\"hello world\"" `shouldParseTo` String "hello world"

      it "parses a string with an escaped quotation mark" $
        "\"hello\\\"world\"" `shouldParseTo` String "hello\"world"

      it "parses a string with multiple escaped quotation marks" $
        "\"he said: \\\"hello\\\"\"" `shouldParseTo` String "he said: \"hello\""

      it "fails when string contains unknown escape sequence" $
        "\"bad\\zescape\"" `shouldFailWithError` "escape sequence \\z"
        
      it "fails on unterminated string" $
        "\"unterminated" `shouldFailWithError` "unexpected end of input"

    describe "parseNumber" $ do
      it "parses a single digit" $
        "5" `shouldParseTo` Number 5

      it "parses multiple digits" $
        "123" `shouldParseTo` Number 123

      it "parses binary numbers" $
        "#b101" `shouldParseTo` Number 5

      it "parses octal numbers" $
        "#o123" `shouldParseTo` Number 83

      it "parses hexadecimal numbers" $
        "#xA1F" `shouldParseTo` Number 2591

      it "parses hexadecimal numbers with lowercase letters" $
        "#xa1f" `shouldParseTo` Number 2591
        
      it "fails on invalid binary number" $
        "#b102" `shouldFailWithError` "invalid number format"
        
      it "fails on invalid octal number" $
        "#o829" `shouldFailWithError` "invalid number format"
        
      it "fails on invalid hexadecimal number" $
        "#xZ12" `shouldFailWithError` "invalid number format"

    describe "parseCharLiteral" $ do
      it "parses a simple character" $
        "#\\a" `shouldParseTo` Character 'a'

      it "parses special characters" $
        "#\\!" `shouldParseTo` Character '!'

      it "parses space character" $
        "#\\space" `shouldParseTo` Character ' '

      it "parses newline character" $
        "#\\newline" `shouldParseTo` Character '\n'

      it "parses tab character" $
        "#\\tab" `shouldParseTo` Character '\t'

      it "fails on unknown character name" $
        "#\\unknown" `shouldFailWithError` "unknown character name"

    describe "parseList" $ do
      it "parses an empty list" $
        "()" `shouldParseTo` List []

      it "parses a list with one element" $
        "(a)" `shouldParseTo` List [Atom "a"]

      it "parses a list with multiple elements" $
        "(a b c)" `shouldParseTo` List [Atom "a", Atom "b", Atom "c"]

      it "parses a nested list" $
        "(a (b c))" `shouldParseTo` List [Atom "a", List [Atom "b", Atom "c"]]

    describe "parseDottedList" $ do
      it "parses a simple dotted list" $
        "(a . b)" `shouldParseTo` DottedList [Atom "a"] (Atom "b")

      it "parses a dotted list with multiple head elements" $
        "(a b c . d)" `shouldParseTo` DottedList [Atom "a", Atom "b", Atom "c"] (Atom "d")
        
      it "fails on incomplete dotted list" $
        "(a . )" `shouldFailWithError` "unexpected"
        
      it "fails when dot is first element" $
        "(. a)" `shouldFailWithError` "unexpected"

    describe "general syntax errors" $ do
      it "fails on unbalanced opening parenthesis" $
        "(a b c" `shouldFailWithError` "unexpected end of input"
        
      it "fails on unbalanced closing parenthesis" $
        "a b c)" `shouldFailWithError` "unexpected"
        
      it "fails on empty input" $
        "" `shouldFailWithError` "unexpected end of input"

    describe "parseQuoted" $ do
      it "parses a quoted atom" $
        "'a" `shouldParseTo` List [Atom "quote", Atom "a"]

      it "parses a quoted list" $
        "'(a b c)" `shouldParseTo` List [Atom "quote", List [Atom "a", Atom "b", Atom "c"]]

    describe "complex expressions" $ do
      it "parses a complex expression" $
        runExcept (doParse "(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))")
          `shouldBe` Right
            ( List
                [ Atom "define",
                  List [Atom "factorial", Atom "n"],
                  List
                    [ Atom "if",
                      List [Atom "=", Atom "n", Number 0],
                      Number 1,
                      List
                        [ Atom "*",
                          Atom "n",
                          List
                            [ Atom "factorial",
                              List [Atom "-", Atom "n", Number 1]
                            ]
                        ]
                    ]
                ]
            )