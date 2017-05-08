module CodeGenerator where

-- lstToString :: [String] -> String
-- lstToString =

functionCall :: [(String, String)] -> String -> String
functionCall [] output = output
functionCall intRep output
    -- Opening tags
    | token == "sTag" && attribute == "startDocument" = functionCall (tail intRep) (output ++ "\\documentclass[11pt]{article}\n\n\\begin{document}")
    | token == "sTag" && attribute == "bold" = functionCall (tail intRep) (output ++ "\\textbf{")
    | token == "sTag" && attribute == "p" = functionCall (tail intRep) (output ++ "\\n")
    | token == "sTag" && attribute == "italics" = functionCall (tail intRep) (output ++ "\\textit{")
    | token == "sTag" && attribute == "em" = functionCall (tail intRep) (output ++ "\\textit{")
    | token == "sTag" && attribute == "pre" = functionCall (tail intRep) (output ++ "\\begin{verbatim}")

    -- Closing tags
    | token == "eTag" && attribute == "pre" = functionCall (tail intRep) (output ++ "\\end{verbatim}")
    | token == "eTag" &&
        attribute == "bold" ||
        attribute == "i" ||
        attribute == "em" = functionCall (tail intRep) (output ++ "}")
    | token == "eTag" && attribute == "endDocument" = functionCall (tail intRep) (output ++ "\\end{document}")

    -- non-tags
    | token == "Word" = functionCall (tail intRep) (output ++ attribute)
    | token == "Lit" = functionCall (tail intRep) (output ++ attribute)
    | token == "Op" = functionCall (tail intRep) (output ++ attribute)
    where
        token = fst $ head intRep
        attribute = snd $ head intRep
