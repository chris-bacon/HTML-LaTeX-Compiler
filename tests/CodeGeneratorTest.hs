module CodeGeneratorTest where

import Test.Hspec

import qualified CodeGenerator as CG

test = hspec $ do
    describe "Code Generator" $ do
        it "returns LaTeX when given 'red' as input" $ do
            Parser.match [("Word", "red")] `shouldBe` "red"
