{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module LexicalAnalyzer where

import Control.Applicative
import Data.Void
import Text.Regex.Posix
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.String as MS
import qualified Text.Megaparsec.Lexer as ML

-- This is the lexical analysis of the compiler, which takes a string and returns a set of lexemes and attributes.
-- Here we are attempting to simulate a deterministic finite automaton.

data HTMLStmt
    = HTMLTag Content
    | HeadTag Content
    | BodyTag Content
    | EmphasisTag Content
    | StrongTag Content
    | BoldTag Content
    | ParaTag Content
    | ItalicsTag Content
    deriving (Show)

data Content = Text String

instance Show (Content) where
    show (Text a) = show a

whitespaceParser :: MS.Parser String
whitespaceParser = M.string " "

-- Naming conventions for HTML tag parsers:
-- Opening tags are named :- tagnameParser
-- Closing tags are named :- tagnameParser'

htmlParser :: MS.Parser String
htmlParser = M.string "<html>"

htmlParser' :: MS.Parser String
htmlParser' = M.string "</html>"

headParser :: MS.Parser String
headParser = M.string "<head>"

headParser' :: MS.Parser String
headParser' = M.string "</head>"

boldParser :: MS.Parser String
boldParser = M.string "<bold>"

boldParser' :: MS.Parser String
boldParser' = M.string "</bold>"

paraParser :: MS.Parser String
paraParser = M.string "<p>"

paraParser' :: MS.Parser String
paraParser' = M.string "</p>"


runParse :: Show a => MS.Parser a -> String -> IO ()
runParse p s = M.parseTest p s

input = "<html></html>"
input2 = "<html>hello</html>"
input3 = "<html><html></html></html>"

run input = runParse htmlStmt input

--stmt = htmlStmt
----    <|> boldStmt
----    <|> paraStmt
----    <|> italicsStmt
--

htmlStmt = do
    htmlParser
--    stmtContent <- stmt
    htmlParser'
    return (HTMLTag (Text stmtContent))

--skipWhitespace :: Parser String
--skipWhitespace = string " "

--mySpace :: Parser ()
--mySpace = L.space skipWhitespace (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

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
