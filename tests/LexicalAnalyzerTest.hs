module LexicalAnalyzerTest where

import Test.Hspec
import Data.List.Split

import qualified LexicalAnalyzer as LA 

testHTML = "<p>the red dog</p>"
testHTML2 = "<p><b>the red dog</b></p>"
inputSingleHTMLTags = "<html></html>"
inputHello = "<html>hello</html>"
inputDoubleHTMLTags = "<html><html></html></html>"
inputParaRedDog = "<html><html><p>This is a red dog</p></html></html>"
inputBoldRed = "<html><b>red</b></html>"
inputWithNewlines = "<html>\n</html>"

testLexicalAnalyzer = hspec $ do
    describe "LexicalAnalyzer" $ do
        describe "lexicalParse" $ do
            it "returns \"HTMLTag (Text \"\")\" for input of \"<html></html>\"" $ do
                let (Right result) = LA.lexicalParse inputSingleHTMLTags
                result `shouldBe` LA.HTMLTag (LA.Text "")
            it "returns \"HTMLTag (HTMLTag (ParaTag (Text \"This is a red dog\")))\" for input of \"<html><html><p>This is a red dog</p></html></html>\"" $ do
                let (Right result) = LA.lexicalParse inputParaRedDog
                result `shouldBe` LA.HTMLTag (LA.HTMLTag (LA.ParaTag (LA.Text "This is a red dog")))
            it "returns \"HTMLTag (BoldTag (Text \"red\")\" for input of \"<html><b>red</b></html>\"" $ do
                let (Right result) = LA.lexicalParse inputBoldRed
                result `shouldBe` LA.HTMLTag (LA.BoldTag (LA.Text "red"))
            it "is able to parse html with newline symbols" $ do
                let (Right result) = LA.lexicalParse inputWithNewlines
                result `shouldBe` LA.HTMLTag (LA.BoldTag (LA.Text "red"))
