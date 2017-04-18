module HTMLLaTeXCompiler where

import HTMLLaTeXParser

addPreamble :: String -> String
addPreamble s = "\\documentclass[11pt]{article}\n\n\\begin{document}" ++ s

addEnding :: String -> String
addEnding s = s ++ "\\end{document}"

addBeginningAndEnding :: String -> String
addBeginningAndEnding = addPreamble . addEnding
