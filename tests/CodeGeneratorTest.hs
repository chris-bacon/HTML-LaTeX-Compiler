module CodeGeneratorTest where

import Test.Hspec

import qualified CodeGenerator as CG

htmlTokens1 = [("sTag", "startDocument"), ("eTag", "endDocument")]

latex1 :: String
latex1 = "\\documentclass[11pt]{article}\n\n\\begin{document}\n\n\\end{document}"

testCodeGenerator = hspec $ do
    describe "Code Generator" $ do
        it "returns LaTeX 'red' when given 'red' as input" $ do
            CG.generateLaTeX [("Word", "red")] `shouldBe` "red"
        it "returns latex1 when given htmlTokens1 as input" $ do
            CG.generateLaTeX htmlTokens1 `shouldBe` latex1 
        it "returns LaTeX '\\textit{red}' when given tokenised version of '<i>red</i>' as input" $ do
            CG.generateLaTeX [("sTag", "italics"), ("Word", "red"), ("eTag", "italics")] `shouldBe` "\\textit{red}"
