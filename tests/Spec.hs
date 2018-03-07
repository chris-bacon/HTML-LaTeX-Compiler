import Test.Hspec
import Data.List.Split

import ParserTest
import LexicalAnalyzerTest

main :: IO ()
main = do
    testParser
    testLexicalAnalyzer
