import Test.Hspec

import qualified Parser as Parser
import qualified LexicalAnalyzer as LA

testHTML :: String
testHTML = "<p><b>red dog.</b></p>\n<p><b>red dog.</b>\n"

main :: IO ()
main = hspec $ do
    describe "LexicalAnalyzer" $ do
        it "isLetterDigit returns true when given a letter" $ do
            LA.isLetterDigit "a" `shouldBe` True
        it "isLetterDigit returns true when given a digit" $ do
            LA.isLetterDigit "1" `shouldBe` True
        it "isLetterDigit returns false when given punctuation" $ do
            LA.isLetterDigit ";" `shouldBe` False
        it "bufferClear returns true when given an empty string" $ do
            LA.bufferClear "" `shouldBe` True
        it "bufferClear returns false when given a non-empty string" $ do
            LA.bufferClear "red" `shouldBe` False

    describe "Parser" $ do
        it "1. consumes head of lexemes when head of lexemes is identical to lookahead" $ do
            Parser.match "sTag" ["sTag", "eTag"] `shouldBe` ["eTag"]
        it "2. consumes head of lexemes when head of lexemes is identical to lookahead" $ do
            Parser.match "sTag" ["sTag", "the", "red", "dog", "eTag"] `shouldBe` ["the", "red", "dog", "eTag"]
