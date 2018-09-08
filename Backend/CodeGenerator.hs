module CodeGenerator where

import LexicalAnalyzer

generateLaTeX :: Stmt -> String
generateLaTeX (HTMLTag stmt) = generateLaTeX stmt
generateLaTeX (HeadTag stmt) = generateLaTeX stmt
generateLaTeX (BodyTag stmt) = generateLaTeX stmt
generateLaTeX (ParaTag stmt) = generateLaTeX stmt ++ "\\"
generateLaTeX (PreTag stmt) = "\\begin{verbatim}" ++ generateLaTeX stmt ++ "\\end{verbatim}"
generateLaTeX (EmphasisTag stmt) = "\\textit{" ++ generateLaTeX stmt ++ "}"
generateLaTeX (StrongTag stmt) = "\\textbf{" ++ generateLaTeX stmt ++ "}"
generateLaTeX (BoldTag stmt) = "\\textbf{" ++ generateLaTeX stmt ++ "}"
generateLaTeX (ItalicsTag stmt) = "\\textit{" ++ generateLaTeX stmt ++ "}"
generateLaTeX (Text stmt) = stmt

preProcessor :: String
preProcessor = "\\documentclass[11pt]{article}\n\n\\begin{document}\n\n"

postProcessor :: String
postProcessor = "\\end{document}"

