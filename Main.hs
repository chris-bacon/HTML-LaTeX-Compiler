module Main where

import System.IO
import System.Environment
import Data.List.Split
-- Frontend
import LexicalAnalyzer
import Parser
-- Backend
-- import CodeGenerator

main :: IO ()
main = do
    (x:xs) <- getArgs
    file <- readFile x
    let inputStream = splitOn "" file
    putStr "Compiling "
    putStr x
    putStrLn "..."
    let tokens = lexAnalyzer inputStream [] []

    let lexemes = map fst tokens
        attributes = map snd tokens
        errors = parse lexemes

    print lexemes
    print attributes

    if length errors == 0 then
        putStrLn "Syntax: well-formed"
    else
        error("Syntax error found...")

    -- codeGenerator tokens

    -- let result1 = addBeginningAndEnding result

    -- writeFile "LaTeX/example2.tex" ?tokens?
    return ()
