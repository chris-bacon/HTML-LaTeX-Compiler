module CodeGeneratorTest where

import Test.Hspec

import qualified CodeGenerator as CG

testCodeGenerator = hspec $ do
    describe "Code Generator" $ do
        it "returns LaTeX 'red' when given 'red' as input" $ do
            CG.generateLaTeX [("Word", "red")] [] `shouldBe` "red"
        it "returns LaTeX '\\textit{red}' when given tokenised version of '<i>red</i>' as input" $ do
            CG.generateLaTeX [("sTag", "italics"), ("Word", "red"), ("eTag", "italics")] [] `shouldBe` "\\textit{red}"
