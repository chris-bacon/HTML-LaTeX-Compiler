import Test.Hspec
import Data.List.Split

import qualified Parser as Parser
import qualified LexicalAnalyzer as LA

testHTML :: String
testHTML = "<p>the red dog</p>"

testHTML2 :: String
testHTML2 = "<p><b>the red dog</b></p>"

main :: IO ()
main = hspec $ do
    describe "LexicalAnalyzer" $ do
        describe "isLetterDigit" $ do
            it "returns true when given a letter" $ do
                LA.isLetterDigit "a" `shouldBe` True
            it "returns true when given a digit" $ do
                LA.isLetterDigit "1" `shouldBe` True
            it "returns false when given punctuation" $ do
                LA.isLetterDigit ";" `shouldBe` False
            it "returns true when given an empty string" $ do
                LA.bufferClear "" `shouldBe` True
            it "returns false when given a non-empty string" $ do
                LA.bufferClear "red" `shouldBe` False
        
        describe "lexAnalyzer" $ do
            it "should tokenize testHTML into lexemes" $ do
                LA.lexAnalyzer (splitOn "" testHTML) `shouldBe` [("sTag", "p"), ("Word", "the"), ("Word", "red"), ("Word", "dog"), ("eTag", "p")]
            it "should tokenize testHTML2 into lexemes" $ do
                LA.lexAnalyzer (splitOn "" testHTML2) `shouldBe` [("sTag", "p"), ("sTag", "bold"), ("Word", "the"), ("Word", "red"), ("Word", "dog"), ("eTag", "bold"), ("eTag", "p")]

    describe "Parser" $ do
        it "1. consumes head of lexemes when head of lexemes is identical to lookahead" $ do
            Parser.match "sTag" ["sTag", "eTag"] `shouldBe` ["eTag"]
        it "2. consumes head of lexemes when head of lexemes is identical to lookahead" $ do
            Parser.match "sTag" ["sTag", "the", "red", "dog", "eTag"] `shouldBe` ["the", "red", "dog", "eTag"]
