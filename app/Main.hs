module Main where

import System.IO
import System.Environment
import Data.List.Split
-- Frontend
import Parser
-- Backend
import CodeGenerator

main :: IO ()
main = do
    [x] <- getArgs
    fileContents <- readFile x
    putStrLn $ "Compiling " ++ x
    let inputStream = splitOn "" fileContents
    let html = parse fileContents
    case html of
      Right t -> do
          putStrLn "Syntax: well-formed"
          let latex = preProcessor ++ generateLaTeX t ++ postProcessor
          putStrLn "LaTeX successfully compiled"
          putStrLn latex
      Left err -> do
          putStrLn $ "Cannot parse input; error:"
          print err

