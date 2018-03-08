module CodeGenerator where

generateLaTeX :: [(String, String)] -> String -> String
generateLaTeX [] output = output
generateLaTeX intRep output
    -- Opening tags
    | token == "sTag" && attribute == "startDocument" = generateLaTeX (tail intRep) (output ++ "\\documentclass[11pt]{article}\n\n\\begin{document}\n\n")
    | token == "sTag" && attribute == "bold" = generateLaTeX (tail intRep) (output ++ "\\textbf{")
    | token == "sTag" && attribute == "italics" = generateLaTeX (tail intRep) (output ++ "\\textit{")
    | token == "sTag" && attribute == "em" = generateLaTeX (tail intRep) (output ++ "\\textit{")
    | token == "sTag" && attribute == "pre" = generateLaTeX (tail intRep) (output ++ "\\begin{verbatim}")

    -- Closing tags
    | token == "eTag" && attribute == "p" = generateLaTeX (tail intRep) (output ++ "\n\n")
    | token == "eTag" && attribute == "pre" = generateLaTeX (tail intRep) (output ++ "\\end{verbatim}")
    | token == "eTag" &&
        attribute == "bold" ||
        attribute == "italics" ||
        attribute == "em" = generateLaTeX (tail intRep) (output ++ "}")
    | token == "eTag" && attribute == "endDocument" = generateLaTeX (tail intRep) (output ++ "\\end{document}")

    -- non-tags
    -- if two words in a sequence, we want whitespace
    | token == "Word" && lookahead == "Word" = generateLaTeX (tail intRep) (output ++ attribute ++ " ")
    | token == "Word" = generateLaTeX (tail intRep) (output ++ attribute)
    | token == "Lit" = generateLaTeX (tail intRep) (output ++ attribute)
    | token == "Op" = generateLaTeX (tail intRep) (output ++ " " ++ attribute)
    -- We may not want to convert every token into a LaTeX representation
    -- And we are assuming that all errors have already been caught by now
    | otherwise = generateLaTeX (tail intRep) output
    where
        token = fst $ head intRep
        lookahead = fst $ f (tail intRep)
        attribute = snd $ head intRep

-- TODO: Refactor away
f [] = ("", "")
f intRep = head intRep
