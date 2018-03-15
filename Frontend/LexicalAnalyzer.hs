{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module LexicalAnalyzer where

import Control.Applicative
import Data.Void
import Text.Regex.Posix
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.String as MS
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Lexer as ML

-- This is the lexical analysis of the compiler, which takes a string and returns a set of lexemes.
-- The following context-free grammar is encoded in the Stmt data structure:
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
contentStmt = some (MC.alphaNumChar <|> MC.char ' ') <|> MC.string "" <|> MC.string "\n" >>= (\x -> return $ Text x)

runParse :: Show a => MS.Parser a -> Input -> Either (M.ParseError (M.Token String) M.Dec) a
runParse p s = M.parse p "" s

lexicalParse input = runParse stmt input

-- OLD 
type Tokens = ([Char], [Char])
type SplitHTML = [String]

--[[:space:]] = whitespace
isLetterDigit :: String -> Bool
isLetterDigit char = char =~ "([a-zA-Z0-9])+" :: Bool

isBufferClear :: String -> Bool
isBufferClear "" = True
isBufferClear _ = False

-- Parse input into lexemes
lexAnalyzer :: SplitHTML -> [Tokens]
lexAnalyzer html = go html [] [] 
    where
        go :: SplitHTML -> [Char] -> [Tokens] -> [Tokens]
        go [] _ output = output
        go (x:xs) buffer output
            -- Word that precedes a tag with no delimiting whitespace
            | (isLetterDigit buffer) && (x == "<") = go xs "<" (output ++ [("Word", buffer)])

            | x == "" = go xs "" output
            | x == "<" = go xs "<" output
            | buffer ++ x == "</" = go xs "</" output

            -- new lines
            | x == "\\" = go xs "\\" output
            | buffer ++ x == "\n" = go xs "" output

            -- bold
            | buffer ++ x == "<b" = go xs "<b" output
            | buffer ++ x == "<b>" = go xs "" (output ++ [("sTag", "bold")])

            | buffer ++ x == "</b" = go xs "</b" output
            | buffer ++ x == "</b>" = go xs "" (output ++ [("eTag", "bold")])

            -- strong
            | buffer ++ x == "<s" = go xs "<s" output
            | buffer ++ x == "<st" = go xs "<st" output
            | buffer ++ x == "<str" = go xs "<str" output
            | buffer ++ x == "<stro" = go xs "<stro" output
            | buffer ++ x == "<stron" = go xs "<stron" output
            | buffer ++ x == "<strong" = go xs "<strong" output
            | buffer ++ x == "<strong>" = go xs "" (output ++ [("sTag", "strong")])

            | buffer ++ x == "<s" = go xs "<s" output
            | buffer ++ x == "<st" = go xs "<st" output
            | buffer ++ x == "<str" = go xs "<str" output
            | buffer ++ x == "<stro" = go xs "<stro" output
            | buffer ++ x == "<stron" = go xs "<stron" output
            | buffer ++ x == "<strong" = go xs "<strong" output
            | buffer ++ x == "<strong>" = go xs "" (output ++ [("eTag", "strong")])

            -- italics
            | buffer ++ x == "<i" = go xs "<i" output
            | buffer ++ x == "<i>" = go xs "" (output ++ [("sTag", "italics")])

            | buffer ++ x == "</i" = go xs "</i" output
            | buffer ++ x == "</i>" = go xs "" (output ++ [("eTag", "italics")])

            -- emphasis
            | buffer ++ x == "<e" = go xs "<e" output
            | buffer ++ x == "<em" = go xs "<em" output
            | buffer ++ x == "<em>" = go xs "" (output ++ [("sTag", "em")])

            | buffer ++ x == "</e" = go xs "</e" output
            | buffer ++ x == "</em" = go xs "</em" output
            | buffer ++ x == "</em>" = go xs "" (output ++ [("eTag", "em")])

            -- paragraph
            | buffer ++ x == "<p" = go xs "<p" output
            | buffer ++ x == "<p>" = go xs "" (output ++ [("sTag", "p")])

            | buffer ++ x == "</p" = go xs "</p" output
            | buffer ++ x == "</p>" = go xs "" (output ++ [("eTag", "p")])

            -- body
            | buffer ++ x == "<b" = go xs "<b" output
            | buffer ++ x == "<bo" = go xs "<bo" output
            | buffer ++ x == "<bod" = go xs "<bod" output
            | buffer ++ x == "<body" = go xs "<body" output
            | buffer ++ x == "<body>" = go xs "" output

            | buffer ++ x == "</b" = go xs "</b" output
            | buffer ++ x == "</bo" = go xs "</bo" output
            | buffer ++ x == "</bod" = go xs "</bod" output
            | buffer ++ x == "</body" = go xs "</body" output
            | buffer ++ x == "</body>" = go xs "" output

            -- pre
            | buffer ++ x == "<p" = go xs "<p" output
            | buffer ++ x == "<pr" = go xs "<pr" output
            | buffer ++ x == "<pre" = go xs "<pre" output
            | buffer ++ x == "<pre>" = go xs "" (output ++ [("sTag", "pre")])

            | buffer ++ x == "</p" = go xs "</p" output
            | buffer ++ x == "</pr" = go xs "</pr" output
            | buffer ++ x == "</pre" = go xs "</pre" output
            | buffer ++ x == "</pre>" = go xs "" (output ++ [("eTag", "pre")])

            -- head
            | buffer ++ x == "<h" = go xs "<h" output
            | buffer ++ x == "<he" = go xs "<he" output
            | buffer ++ x == "<hea" = go xs "<hea" output
            | buffer ++ x == "<head" = go xs "<head" output
            | buffer ++ x == "<head>" = go xs "" output

            | buffer ++ x == "</h" = go xs "</h" output
            | buffer ++ x == "</he" = go xs "</he" output
            | buffer ++ x == "</hea" = go xs "</hea" output
            | buffer ++ x == "</head" = go xs "</head" output
            | buffer ++ x == "</head>" = go xs "" output

            -- html
            | buffer ++ x == "<h" = go xs "<h" output
            | buffer ++ x == "<ht" = go xs "<ht" output
            | buffer ++ x == "<htm" = go xs "<htm" output
            | buffer ++ x == "<html" = go xs "<html" output
            | buffer ++ x == "<html>" = go xs "" (output ++ [("sTag", "startDocument")])

            | buffer ++ x == "</h" = go xs "</h" output
            | buffer ++ x == "</ht" = go xs "</ht" output
            | buffer ++ x == "</htm" = go xs "</htm" output
            | buffer ++ x == "</html" = go xs "</html" output
            | buffer ++ x == "</html>" = go xs "" (output ++ [("eTag", "endDocument")])

            -- Operators
            | x == ">" = go xs "" (output ++ [("Op", ">")])
            | x == "=" = go xs "" (output ++ [("Op", "=")])
            | x == "+" = go xs "" (output ++ [("Op", "+")])
            | x == "-" = go xs "" (output ++ [("Op", "-")])
            | x == "*" = go xs "" (output ++ [("Op", "*")])
            | x == "/" = go xs "" (output ++ [("Op", "/")])
            | buffer ++ x == "<=" = go xs "" (output ++ [("Op", "<=")])
            | buffer ++ x == "<>" = go xs "" (output ++ [("Op", "<>")])
            | buffer ++ x == "=>" = go xs "" (output ++ [("Op", "=>")])

            | x == "(" = go xs "" (output ++ [("Lit", "(")])
            | x == ")" && isBufferClear buffer = go xs "" (output ++ [("Lit", ")")])
            | x == ")" = go xs "" (output ++ [("Word", buffer), ("Lit", ")")])
            -- Alphanumeric
            | buffer ++ x == buffer ++ " " = go xs "" (output ++ [("Word", buffer)])
            | buffer ++ x == buffer ++ "!" = go xs "" (output ++ [("Word", buffer), ("Lit", "!")])
            | buffer ++ x == buffer ++ "." = go xs "" (output ++ [("Word", buffer), ("Lit", ".")])
            | buffer ++ x == buffer ++ "," = go xs "" (output ++ [("Word", buffer), ("Lit", ",")])
            | buffer ++ x == buffer ++ ";" = go xs "" (output ++ [("Word", buffer), ("Lit", ";")])
            | isLetterDigit (buffer ++ x) = go xs (buffer ++ x) output
            -- x
            -- | otherwise = go xs "" (output ++ [("Unkown", x)])
            | otherwise = error("Input could not be parsed")
