import Test.Hspec

import ParserTest
import LexicalAnalyzerTest

main :: IO ()
main = do
    testParser
    testLexicalAnalyzer
