module HTMLLaTeXCompiler where

import HTMLLaTeXParser

addBeginning :: String -> String
addBeginning s = "\\documentclass[11pt]{article}\n\n\\begin{document}" ++ s

addEnding :: String -> String
addEnding s = s ++ "\\end{document}"

addBeginningAndEnding :: String -> String
addBeginningAndEnding = addBeginning . addEnding
