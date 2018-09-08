module Main where

import System.IO
import System.Environment
import Data.List.Split
-- Frontend
import LexicalAnalyzer
import Parser
-- Backend
import CodeGenerator

main :: IO ()
main = do
    [x] <- getArgs
    fileContents <- readFile x
    let inputStream = splitOn "" fileContents
    let html = lexicalParse fileContents
    case html of
      Right t -> do
          let latex = preProcessor ++ generateLaTeX t ++ postProcessor
          print latex
      Left err -> print err
    --putStrLn $ "Compiling " ++ x ++ "..."
    --let tokens = lexAnalyzer inputStream
    --let lexemes = map fst tokens
    --    attributes = map snd tokens
    --    errors = parse lexemes

    ---- Testing only
    ----print lexemes
    ----print attributes

    --if length errors == 0 then
    --    putStrLn "Syntax: well-formed"
    --else
    --    error("Syntax error found...")

    --let latex = generateLaTeX tokens

    --writeFile "LaTeX/example2.tex" latex
    --putStrLn "LaTeX successfully compiled"
    return ()
