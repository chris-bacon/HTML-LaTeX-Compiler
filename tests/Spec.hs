import Test.Hspec

import qualified Parser as Parser

testHTML :: String
testHTML = "<p><b>red dog.</b></p>\n<p><b>red dog.</b>\n"


match :: String -> [String] -> [String]
match lookahead [] = error("Syntax Error... Possibily missing tag")
match lookahead lexemes
    | lookahead == terminal = nextTerminal
    | otherwise = error("Syntax Error... Mismatch: (1) "++ lookahead ++ " (2) " ++ terminal)
    where
        terminal = head lexemes
        nextTerminal = tail lexemes



main :: IO ()
main = hspec $ do
    describe "Parser" $ do
        it "1. consumes head of lexemes when head of lexemes is identical to lookahead" $ do
            Parser.match "sTag" ["sTag", "eTag"] `shouldBe` ["eTag"]
        it "2. consumes head of lexemes when head of lexemes is identical to lookahead" $ do
            Parser.match "sTag" ["sTag", "the", "red", "dog", "eTag"] `shouldBe` ["the", "red", "dog", "eTag"]
