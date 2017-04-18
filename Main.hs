module Main where

import System.IO
import Data.List.Split
-- Frontend
import LexicalAnalyzer
import Parser
import ICG
-- Backend
import CodeGenerator

-- import HTMLLaTeXParser
-- import HTMLLaTeXCompiler

main :: IO ()
main = do
    file <- readFile "HTML/example.html"
    let stringList = splitOn "" file
    let result = construct stringList "" ""
    let result1 = addBeginningAndEnding result
    print result1
    writeFile "example.tex" result1
    return ()
