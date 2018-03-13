import Test.Hspec

import ParserTest
import LexicalAnalyzerTest
--import CodeGeneratorTest 

main :: IO ()
main = do
    testParser
    testLexicalAnalyzer
--    testCodeGenerator
