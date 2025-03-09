module CommonSpec (commonSpec) where

import Lib (LispVal (..))
import Test.Hspec (Spec, describe, it, shouldBe)

commonSpec :: Spec
commonSpec = do
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
      show (Character ' ') `shouldBe` "#\\space"
      show (Character '\n') `shouldBe` "#\\newline"

    it "shows an empty list" $
      show (List []) `shouldBe` "()"

    it "shows a simple list" $
      show (List [Atom "a", Atom "b", Atom "c"]) `shouldBe` "(a b c)"

    it "shows a nested list" $
      show (List [Atom "a", List [Atom "b", Atom "c"]]) `shouldBe` "(a (b c))"

    it "shows a dotted list" $
      show (DottedList [Atom "a", Atom "b"] (Atom "c")) `shouldBe` "(a b . c)"

    it "shows a complex expression" $
      show
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
        `shouldBe` "(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))"