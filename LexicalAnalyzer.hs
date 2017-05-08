{-# LANGUAGE FlexibleContexts #-}

module LexicalAnalyzer where

import Text.Regex.Posix

-- This is the lexical analysis of the compiler, which takes a string and returns a set of lexemes and attributes.
-- Here we are attempting to simulate a deterministic finite automaton.

type Tokens = ([Char], [Char])
--[[:space:]] = whitespace
isLetterDigit :: String -> Bool
isLetterDigit char = char =~ "([a-zA-Z0-9])+" :: Bool

-- Parse input into lexemes
lexAnalyzer :: [[Char]] -> [Char] -> [Tokens] -> [Tokens]
lexAnalyzer [] buffer output = output
lexAnalyzer (x:xs) buffer output
    -- Word that precedes a tag with no delimiting whitespace
    | (isLetterDigit buffer) && (x == "<") = lexAnalyzer xs "<" (output ++ [("Word", buffer)])

    | x == "<" = lexAnalyzer xs "<" output
    | buffer ++ x == "</" = lexAnalyzer xs "</" output

    -- new lines
    | x == "\\" = lexAnalyzer xs "\\" output
    | buffer ++ x == "\n" = lexAnalyzer xs "" output

    -- bold
    | buffer ++ x == "<b" = lexAnalyzer xs "<b" output
    | buffer ++ x == "<b>" = lexAnalyzer xs "" (output ++ [("sTag", "bold")])

    | buffer ++ x == "</b" = lexAnalyzer xs "</b" output
    | buffer ++ x == "</b>" = lexAnalyzer xs "" (output ++ [("eTag", "bold")])

    -- strong
    | buffer ++ x == "<s" = lexAnalyzer xs "<s" output
    | buffer ++ x == "<st" = lexAnalyzer xs "<st" output
    | buffer ++ x == "<str" = lexAnalyzer xs "<str" output
    | buffer ++ x == "<stro" = lexAnalyzer xs "<stro" output
    | buffer ++ x == "<stron" = lexAnalyzer xs "<stron" output
    | buffer ++ x == "<strong" = lexAnalyzer xs "<strong" output
    | buffer ++ x == "<strong>" = lexAnalyzer xs "" (output ++ [("sTag", "strong")])

    | buffer ++ x == "<s" = lexAnalyzer xs "<s" output
    | buffer ++ x == "<st" = lexAnalyzer xs "<st" output
    | buffer ++ x == "<str" = lexAnalyzer xs "<str" output
    | buffer ++ x == "<stro" = lexAnalyzer xs "<stro" output
    | buffer ++ x == "<stron" = lexAnalyzer xs "<stron" output
    | buffer ++ x == "<strong" = lexAnalyzer xs "<strong" output
    | buffer ++ x == "<strong>" = lexAnalyzer xs "" (output ++ [("eTag", "strong")])

    -- italics
    | buffer ++ x == "<i" = lexAnalyzer xs "<i" output
    | buffer ++ x == "<i>" = lexAnalyzer xs "" (output ++ [("sTag", "italics")])

    | buffer ++ x == "</i" = lexAnalyzer xs "</i" output
    | buffer ++ x == "</i>" = lexAnalyzer xs "" (output ++ [("eTag", "italics")])

    -- emphasis
    | buffer ++ x == "<e" = lexAnalyzer xs "<e" output
    | buffer ++ x == "<em" = lexAnalyzer xs "<em" output
    | buffer ++ x == "<em>" = lexAnalyzer xs "" (output ++ [("sTag", "em")])

    | buffer ++ x == "</e" = lexAnalyzer xs "</e" output
    | buffer ++ x == "</em" = lexAnalyzer xs "</em" output
    | buffer ++ x == "</em>" = lexAnalyzer xs "" (output ++ [("eTag", "em")])

    -- paragraph
    | buffer ++ x == "<p" = lexAnalyzer xs "<p" output
    | buffer ++ x == "<p>" = lexAnalyzer xs "" (output ++ [("sTag", "p")])

    | buffer ++ x == "</p" = lexAnalyzer xs "</p" output
    | buffer ++ x == "</p>" = lexAnalyzer xs "" (output ++ [("eTag", "p")])

    -- body
    | buffer ++ x == "<b" = lexAnalyzer xs "<b" output
    | buffer ++ x == "<bo" = lexAnalyzer xs "<bo" output
    | buffer ++ x == "<bod" = lexAnalyzer xs "<bod" output
    | buffer ++ x == "<body" = lexAnalyzer xs "<body" output
    | buffer ++ x == "<body>" = lexAnalyzer xs "" output

    | buffer ++ x == "</b" = lexAnalyzer xs "</b" output
    | buffer ++ x == "</bo" = lexAnalyzer xs "</bo" output
    | buffer ++ x == "</bod" = lexAnalyzer xs "</bod" output
    | buffer ++ x == "</body" = lexAnalyzer xs "</body" output
    | buffer ++ x == "</body>" = lexAnalyzer xs "" output

    -- pre
    | buffer ++ x == "<p" = lexAnalyzer xs "<p" output
    | buffer ++ x == "<pr" = lexAnalyzer xs "<pr" output
    | buffer ++ x == "<pre" = lexAnalyzer xs "<pre" output
    | buffer ++ x == "<pre>" = lexAnalyzer xs "" (output ++ [("sTag", "pre")])

    | buffer ++ x == "</p" = lexAnalyzer xs "</p" output
    | buffer ++ x == "</pr" = lexAnalyzer xs "</pr" output
    | buffer ++ x == "</pre" = lexAnalyzer xs "</pre" output
    | buffer ++ x == "</pre>" = lexAnalyzer xs "" (output ++ [("eTag", "pre")])

    -- head
    | buffer ++ x == "<h" = lexAnalyzer xs "<h" output
    | buffer ++ x == "<he" = lexAnalyzer xs "<he" output
    | buffer ++ x == "<hea" = lexAnalyzer xs "<hea" output
    | buffer ++ x == "<head" = lexAnalyzer xs "<head" output
    | buffer ++ x == "<head>" = lexAnalyzer xs "" output

    | buffer ++ x == "</h" = lexAnalyzer xs "</h" output
    | buffer ++ x == "</he" = lexAnalyzer xs "</he" output
    | buffer ++ x == "</hea" = lexAnalyzer xs "</hea" output
    | buffer ++ x == "</head" = lexAnalyzer xs "</head" output
    | buffer ++ x == "</head>" = lexAnalyzer xs "" output

    -- html
    | buffer ++ x == "<h" = lexAnalyzer xs "<h" output
    | buffer ++ x == "<ht" = lexAnalyzer xs "<ht" output
    | buffer ++ x == "<htm" = lexAnalyzer xs "<htm" output
    | buffer ++ x == "<html" = lexAnalyzer xs "<html" output
    | buffer ++ x == "<html>" = lexAnalyzer xs "" (output ++ [("sTag", "document")])

    | buffer ++ x == "</h" = lexAnalyzer xs "</h" output
    | buffer ++ x == "</ht" = lexAnalyzer xs "</ht" output
    | buffer ++ x == "</htm" = lexAnalyzer xs "</htm" output
    | buffer ++ x == "</html" = lexAnalyzer xs "</html" output
    | buffer ++ x == "</html>" = lexAnalyzer xs "" (output ++ [("eTag", "document")])

    -- Operators
    | x == ">" = lexAnalyzer xs "" (output ++ [("Op", ">")])
    | x == "=" = lexAnalyzer xs "" (output ++ [("Op", "=")])
    | buffer ++ x == "<=" = lexAnalyzer xs "" (output ++ [("Op", "<=")])
    | buffer ++ x == "<>" = lexAnalyzer xs "" (output ++ [("Op", "<>")])
    | buffer ++ x == "=>" = lexAnalyzer xs "" (output ++ [("Op", "=>")])

    -- Alphanumeric
    | buffer ++ x == buffer ++ " " = lexAnalyzer xs "" (output ++ [("Word", buffer)])
    | buffer ++ x == buffer ++ "!" = lexAnalyzer xs "" (output ++ [("Word", buffer), ("Lit", "!")])
    | buffer ++ x == buffer ++ "." = lexAnalyzer xs "" (output ++ [("Word", buffer), ("Lit", ".")])
    | buffer ++ x == buffer ++ "," = lexAnalyzer xs "" (output ++ [("Word", buffer), ("Lit", ",")])
    | buffer ++ x == buffer ++ ";" = lexAnalyzer xs "" (output ++ [("Word", buffer), ("Lit", ";")])
    | isLetterDigit (buffer ++ x) = lexAnalyzer xs (buffer ++ x) output

    -- x
    | otherwise = lexAnalyzer xs "" (output ++ [("Unkown", x)])
    -- | otherwise = error("some error")
