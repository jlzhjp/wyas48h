module ParserSpec (parserSpec) where

import Lib (LispVal (..), doParse)
import Test.Hspec
  ( Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
    shouldContain,
  )

parserSpec :: Spec
parserSpec = do
  describe "Parser" $ do
    describe "parseAtom" $ do
      it "parses a simple atom" $
        doParse "abc" `shouldBe` Right (Atom "abc")

      it "parses an atom with symbols" $
        doParse "a-b_c!$%&" `shouldBe` Right (Atom "a-b_c!$%&")

      it "parses boolean true" $
        doParse "#t" `shouldBe` Right (Bool True)

      it "parses boolean false" $
        doParse "#f" `shouldBe` Right (Bool False)

    describe "parseString" $ do
      it "parses a simple string" $
        doParse "\"hello\"" `shouldBe` Right (String "hello")

      it "parses an empty string" $
        doParse "\"\"" `shouldBe` Right (String "")

      it "parses a string with spaces" $
        doParse "\"hello world\"" `shouldBe` Right (String "hello world")

      it "parses a string with an escaped quotation mark" $
        doParse "\"hello\\\"world\"" `shouldBe` Right (String "hello\"world")

      it "parses a string with multiple escaped quotation marks" $
        doParse "\"he said: \\\"hello\\\"\"" `shouldBe` Right (String "he said: \"hello\"")

      it "fails when string contains unknown escape sequence" $
        case doParse "\"bad\\zescape\"" of
          Left err -> show err `shouldContain` "escape sequence \\z"
          Right _ -> expectationFailure "expected parse to fail"

    describe "parseNumber" $ do
      it "parses a single digit" $
        doParse "5" `shouldBe` Right (Number 5)

      it "parses multiple digits" $
        doParse "123" `shouldBe` Right (Number 123)
        
      it "parses binary numbers" $
        doParse "#b101" `shouldBe` Right (Number 5)
        
      it "parses octal numbers" $
        doParse "#o123" `shouldBe` Right (Number 83)
        
      it "parses hexadecimal numbers" $
        doParse "#xA1F" `shouldBe` Right (Number 2591)
        
      it "parses hexadecimal numbers with lowercase letters" $
        doParse "#xa1f" `shouldBe` Right (Number 2591)

    describe "parseList" $ do
      it "parses an empty list" $
        doParse "()" `shouldBe` Right (List [])

      it "parses a list with one element" $
        doParse "(a)" `shouldBe` Right (List [Atom "a"])

      it "parses a list with multiple elements" $
        doParse "(a b c)"
          `shouldBe` Right (List [Atom "a", Atom "b", Atom "c"])

      it "parses a nested list" $
        doParse "(a (b c))"
          `shouldBe` Right (List [Atom "a", List [Atom "b", Atom "c"]])

    describe "parseDottedList" $ do
      it "parses a simple dotted list" $
        doParse "(a . b)"
          `shouldBe` Right (DottedList [Atom "a"] (Atom "b"))

      it "parses a dotted list with multiple head elements" $
        doParse "(a b c . d)"
          `shouldBe` Right (DottedList [Atom "a", Atom "b", Atom "c"] (Atom "d"))

    describe "parseQuoted" $ do
      it "parses a quoted atom" $
        doParse "'a"
          `shouldBe` Right (List [Atom "quote", Atom "a"])

      it "parses a quoted list" $
        doParse "'(a b c)"
          `shouldBe` Right (List [Atom "quote", List [Atom "a", Atom "b", Atom "c"]])

    describe "complex expressions" $ do
      it "parses a complex expression" $
        doParse "(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))"
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
