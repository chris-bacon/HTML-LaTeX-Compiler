module LexicalAnalyzerTest where

import Test.Hspec
import Data.List.Split

import qualified LexicalAnalyzer as LA 

testHTML = "<p>the red dog</p>"
testHTML2 = "<p><b>the red dog</b></p>"
input1 = "<html></html>"
input2 = "<html>hello</html>"
input3 = "<html><html></html></html>"
input4 = "<html><html><p>This is a red dog</p></html></html>"

testLexicalAnalyzer = hspec $ do
    describe "LexicalAnalyzer" $ do
        describe "run" $ do
            it "returns \"HTMLTag (HTMLTag (ParaTag (Text \"This is a red dog\")))\" for input of \"<html><html><p>This is a red dog</p></html></html>\"" $ do
                let (Right result) = LA.run input4
                result `shouldBe` LA.HTMLTag (LA.HTMLTag (LA.ParaTag (LA.Text "This is a red dog")))
        -- describe "isLetterDigit" $ do
        --     it "returns true when given a letter" $ do
        --         LA.isLetterDigit "a" `shouldBe` True
        --     it "returns true when given a digit" $ do
        --         LA.isLetterDigit "1" `shouldBe` True
        --     it "returns false when given punctuation" $ do
        --         LA.isLetterDigit ";" `shouldBe` False
        --     it "returns true when given an empty string" $ do
        --         LA.isBufferClear "" `shouldBe` True
        --     it "returns false when given a non-empty string" $ do
        --         LA.isBufferClear "red" `shouldBe` False
        -- describe "lexAnalyzer" $ do
        --     it "should tokenize testHTML into lexemes" $ do
        --         LA.lexAnalyzer (splitOn "" testHTML) `shouldBe` [("sTag", "p"), ("Word", "the"), ("Word", "red"), ("Word", "dog"), ("eTag", "p")]
        --     it "should tokenize testHTML2 into lexemes" $ do
        --         LA.lexAnalyzer (splitOn "" testHTML2) `shouldBe` [("sTag", "p"), ("sTag", "bold"), ("Word", "the"), ("Word", "red"), ("Word", "dog"), ("eTag", "bold"), ("eTag", "p")]

