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

    describe "parseCharLiteral" $ do
      it "parses a simple character" $
        doParse "#\\a" `shouldBe` Right (Character 'a')
        
      it "parses special characters" $
        doParse "#\\!" `shouldBe` Right (Character '!')
        
      it "parses space character" $
        doParse "#\\space" `shouldBe` Right (Character ' ')
        
      it "parses newline character" $
        doParse "#\\newline" `shouldBe` Right (Character '\n')
        
      it "parses tab character" $
        doParse "#\\tab" `shouldBe` Right (Character '\t')
        
      -- it "fails on unknown character name" $
      --   case doParse "#\\unknown" of
      --     Left err -> show err `shouldContain` "unknown character name"
      --     Right val -> expectationFailure $ "expected parse to fail, got " ++ show val

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

    describe "Show instance for LispVal" $ do
      it "shows an atom" $
        show (Atom "abc") `shouldBe` "abc"

      it "shows a number" $
        show (Number 123) `shouldBe` "123"

      it "shows a string" $
        show (String "hello world") `shouldBe` "\"hello world\""

      it "shows a boolean true" $
        show (Bool True) `shouldBe` "#t"

      it "shows a boolean false" $
        show (Bool False) `shouldBe` "#f"

      it "shows a character" $
        show (Character 'a') `shouldBe` "#\\a"

      it "shows special characters" $ do
        show (Character ' ') `shouldBe` "#\\ "
        show (Character '\n') `shouldBe` "#\\\n"

      it "shows an empty list" $
        show (List []) `shouldBe` "()"

      it "shows a simple list" $
        show (List [Atom "a", Atom "b", Atom "c"]) `shouldBe` "(a b c)"

      it "shows a nested list" $
        show (List [Atom "a", List [Atom "b", Atom "c"]]) `shouldBe` "(a (b c))"

      it "shows a dotted list" $
        show (DottedList [Atom "a", Atom "b"] (Atom "c")) `shouldBe` "(a b . c)"

      it "shows a complex expression" $
        show (List [ Atom "define",
                    List [Atom "factorial", Atom "n"],
                    List [ Atom "if",
                          List [Atom "=", Atom "n", Number 0],
                          Number 1,
                          List [ Atom "*",
                                Atom "n",
                                List [ Atom "factorial",
                                      List [Atom "-", Atom "n", Number 1]
                                    ]
                              ]
                        ]
                  ]) `shouldBe` "(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))"
