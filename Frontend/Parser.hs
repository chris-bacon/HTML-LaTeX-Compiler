{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Parser where

import Control.Applicative
import Data.Void
import Text.Regex.Posix
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.String as MS
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Lexer as ML

-- S ::= html | p | content | ...
-- html ::= <html>S</html>
-- p ::= <p>S</p>
-- ...
-- content ::= alphaNumChar | " " | ...

data Stmt
    = HTMLTag Stmt
    | HeadTag Stmt
    | BodyTag Stmt
    | ParaTag Stmt
    | PreTag Stmt
    | EmphasisTag Stmt
    | StrongTag Stmt
    | BoldTag Stmt
    | ItalicsTag Stmt
    | Text String
    deriving (Show, Eq)

type Input = String

stmt :: MS.Parser Stmt
stmt = tagStmt "html" HTMLTag 
    <|> tagStmt "head" HeadTag 
    <|> tagStmt "body" BodyTag
    <|> tagStmt "p" ParaTag
    <|> tagStmt "pre" PreTag
    <|> tagStmt "em" EmphasisTag
    <|> tagStmt "strong" StrongTag
    <|> tagStmt "b" BoldTag
    <|> tagStmt "i" ItalicsTag
    <|> contentStmt

tagStmt :: String -> (Stmt -> Stmt) -> MS.Parser Stmt
tagStmt htmlNode tagName = do
    M.string $ concat ["<", htmlNode, ">"]
    stmtContent <- stmt
    M.string $ concat ["</", htmlNode, ">"]
    return $ tagName (stmtContent)

contentStmt :: MS.Parser Stmt
contentStmt = some 
    (MC.alphaNumChar 
    <|> MC.spaceChar
    <|> MC.newline
    <|> MC.punctuationChar
    <|> MC.symbolChar
    )
    <|> MC.eol
    <|> MC.string "" >>= (\x -> return $ Text x)

runParse :: Show a => MS.Parser a -> Input -> Either (M.ParseError (M.Token String) M.Dec) a
runParse p s = M.parse p "" s

parse input = runParse stmt input

