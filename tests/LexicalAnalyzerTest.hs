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
input5 = "<html><b>red</b></html>"

testLexicalAnalyzer = hspec $ do
    describe "LexicalAnalyzer" $ do
        describe "run" $ do
            it "returns \"HTMLTag (Text \"\")\" for input of \"<html></html>\"" $ do
                let (Right result) = LA.run input1
                result `shouldBe` LA.HTMLTag (LA.Text "")
            it "returns \"HTMLTag (HTMLTag (ParaTag (Text \"This is a red dog\")))\" for input of \"<html><html><p>This is a red dog</p></html></html>\"" $ do
                let (Right result) = LA.run input4
                result `shouldBe` LA.HTMLTag (LA.HTMLTag (LA.ParaTag (LA.Text "This is a red dog")))
            it "returns \"HTMLTag (BoldTag (Text \"red\")\" for input of \"<html><b>red</b></html>\"" $ do
                let (Right result) = LA.run input5
                result `shouldBe` LA.HTMLTag (LA.BoldTag (LA.Text "red"))