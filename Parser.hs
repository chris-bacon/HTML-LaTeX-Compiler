module Parser where

-- This is a recursive descent parser, where each context-free production rule is represented by a function
-- This can also be considered a formal definition of HTML, as accepted by this compiler.
-- The parser is where syntacic and semantic analysis is usually carried out, but this parse, for the moment,
-- only handles syntactic analysis.

-- lookahead is the next lexeme
parse :: [String] -> [String]
parse [] = []
parse lexemes
    | lookahead == "sTag" = parse $ tag lexemes
    | lookahead == "Word" = parse $ match "Word" lexemes
    | lookahead == "Op" = parse $ match "Op" lexemes
    | lookahead == "eTag" = lexemes
    | lookahead == "Lit" = parse $ match "Lit" lexemes
    | otherwise = error("Missing Lexeme or invalid token")
    where
        lookahead = head lexemes

-- Production rule: tag -> sTag Lexemes eTag
-- This rule simulates a syntax tree where a node has two child nodes (an sTag and an eTag)
-- or three child nodes (an sTag, another nonterminal node, and an eTag)
tag lexemes = match "eTag" (parse (match "sTag" lexemes))

-- If a match is successful "execute" the terminal by moving on to the next lexeme
match :: String -> [String] -> [String]
match lookahead [] = error("Syntax Error... Possibily missing tag")
match lookahead lexemes
    | lookahead == terminal = nextTerminal
    | otherwise = error("Syntax Error... Mismatch: (1) "++ lookahead ++ " (2) " ++ terminal)
    where
        terminal = head lexemes
        nextTerminal = tail lexemes
