module Parser where

-- Syntactic and Semantics analysis

-- This is a recursive descent parser, where each context-free production rule is represented by a function
-- This can also be considered a formal definition of HTML, as accepted by this compiler.

-- CFG for HTML
-- tag -> tagStart X tagEnd
-- X -> a|..|z|1|..|9|tag|X

-- lookahead is the next lexeme
parse :: [String] -> [String]
parse [] = []
parse lexemes
    | lookahead == "sTag" = parse $ tag lexemes
    | lookahead == "Word" = parse $ match "Word" lexemes
    | lookahead == "Op" = parse $ match "Op" lexemes
    | lookahead == "eTag" = lexemes
    | lookahead == "Lit" = parse $ match "Lit" lexemes
    | lookahead == "Unkown" = parse $ match "Unkown" lexemes -- TODO: TESTING ONLY
    | otherwise = error("Missing Lexeme or invalid token")
    where
        lookahead = head lexemes

-- Production rule: tag -> sTag Lexemes eTag
-- This rule simulates a syntax tree where a node has two child nodes (an sTag and an eTag)
-- or three child nodes (an sTag, another nonterminal node, and an eTag)
tag lexemes = match "eTag" (parse (match "sTag" lexemes))

-- If a match is successful "execute" the terminal
match :: String -> [String] -> [String]
match lookahead [] = error("Syntax Error... Possibily missing tag")
match lookahead lexemes
    | lookahead == terminal = nextTerminal
    | otherwise = error("Syntax Error... Mismatch: (1) "++ lookahead ++ " (2) " ++ terminal)
    where
        terminal = head lexemes
        nextTerminal = tail lexemes
