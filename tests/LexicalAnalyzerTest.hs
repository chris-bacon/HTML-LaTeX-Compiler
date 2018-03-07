module LexicalAnalyzerTest where

import Test.Hspec
import Data.List.Split

import qualified LexicalAnalyzer as LA 

testHTML :: String
testHTML = "<p>the red dog</p>"

testHTML2 :: String
testHTML2 = "<p><b>the red dog</b></p>"

testLexicalAnalyzer = hspec $ do
    describe "LexicalAnalyzer" $ do
        describe "isLetterDigit" $ do
            it "returns true when given a letter" $ do
                LA.isLetterDigit "a" `shouldBe` True
            it "returns true when given a digit" $ do
                LA.isLetterDigit "1" `shouldBe` True
            it "returns false when given punctuation" $ do
                LA.isLetterDigit ";" `shouldBe` False
            it "returns true when given an empty string" $ do
                LA.isBufferClear "" `shouldBe` True
            it "returns false when given a non-empty string" $ do
                LA.isBufferClear "red" `shouldBe` False
        describe "lexAnalyzer" $ do
            it "should tokenize testHTML into lexemes" $ do
                LA.lexAnalyzer (splitOn "" testHTML) `shouldBe` [("sTag", "p"), ("Word", "the"), ("Word", "red"), ("Word", "dog"), ("eTag", "p")]
            it "should tokenize testHTML2 into lexemes" $ do
                LA.lexAnalyzer (splitOn "" testHTML2) `shouldBe` [("sTag", "p"), ("sTag", "bold"), ("Word", "the"), ("Word", "red"), ("Word", "dog"), ("eTag", "bold"), ("eTag", "p")]

