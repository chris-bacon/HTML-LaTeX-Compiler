module CodeGenerator where

generateLaTeX :: [(String, String)] -> String
generateLaTeX intRep = go intRep "" 
    where
        go :: [(String, String)] -> String -> String
        go [] output = output
        go intRep output
            -- Opening tags
            | token == "sTag" && attribute == "startDocument" = go (tail intRep) (output ++ "\\documentclass[11pt]{article}\n\n\\begin{document}\n\n")
            | token == "sTag" && attribute == "bold" = go (tail intRep) (output ++ "\\textbf{")
            | token == "sTag" && attribute == "italics" = go (tail intRep) (output ++ "\\textit{")
            | token == "sTag" && attribute == "em" = go (tail intRep) (output ++ "\\textit{")
            | token == "sTag" && attribute == "pre" = go (tail intRep) (output ++ "\\begin{verbatim}")

            -- Closing tags
            | token == "eTag" && attribute == "p" = go (tail intRep) (output ++ "\n\n")
            | token == "eTag" && attribute == "pre" = go (tail intRep) (output ++ "\\end{verbatim}")
            | token == "eTag" &&
                attribute == "bold" ||
                attribute == "italics" ||
                attribute == "em" = go (tail intRep) (output ++ "}")
            | token == "eTag" && attribute == "endDocument" = go (tail intRep) (output ++ "\\end{document}")

            -- non-tags
            -- if two words in a sequence, we want whitespace
            | token == "Word" && lookahead == "Word" = go (tail intRep) (output ++ attribute ++ " ")
            | token == "Word" = go (tail intRep) (output ++ attribute)
            | token == "Lit" = go (tail intRep) (output ++ attribute)
            | token == "Op" = go (tail intRep) (output ++ " " ++ attribute)
            -- We may not want to convert every token into a LaTeX representation
            -- And we are assuming that all errors have already been caught by now
            | otherwise = go (tail intRep) output
            where
                token = fst $ head intRep
                lookahead = fst $ f (tail intRep)
                attribute = snd $ head intRep

-- TODO: Refactor away
f [] = ("", "")
f intRep = head intRep
