module ParserTest where

import Test.Hspec

import qualified Parser as Parser

testParser = hspec $ do
    describe "Parser" $ do
        it "1. consumes head of lexemes when head of lexemes is identical to lookahead" $ do
            Parser.match "sTag" ["sTag", "eTag"] `shouldBe` ["eTag"]
        it "2. consumes head of lexemes when head of lexemes is identical to lookahead" $ do
            Parser.match "sTag" ["sTag", "the", "red", "dog", "eTag"] `shouldBe` ["the", "red", "dog", "eTag"]
