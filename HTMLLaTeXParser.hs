module HTMLLaTeXParser where

construct :: [[Char]] -> [Char] -> [Char] -> [Char]
construct [] storedValue final = []
construct (x:xs) storedValue final
    | x == "<" = construct xs "<" final
    | storedValue ++ x == "</" = construct xs "</" final
    -- bold
    | storedValue ++ x == "<b" = construct xs "<b" final
    | storedValue ++ x == "<b>" = "\\textbf{" ++ construct xs "" final

    | storedValue ++ x == "</b" = construct xs "</b" final
    | storedValue ++ x == "</b>" = "}" ++ construct xs "" final
    -- italics
    | storedValue ++ x == "<i" = construct xs "<i" final
    | storedValue ++ x == "<i>" = "\\textit{" ++ construct xs "" final

    | storedValue ++ x == "</i" = construct xs "</i" final
    | storedValue ++ x == "</i>" = "}" ++ construct xs "" final
    -- emphasis
    | storedValue ++ x == "<e" = construct xs "<e" final
    | storedValue ++ x == "<em" = construct xs "<em" final
    | storedValue ++ x == "<em>" = "\\textit{" ++ construct xs "" final

    | storedValue ++ x == "</e" = construct xs "</e" final
    | storedValue ++ x == "</em" = construct xs "</em" final
    | storedValue ++ x == "</em>" = "}" ++ construct xs "" final

    -- paragraph
    | storedValue ++ x == "<p" = construct xs "<p" final
    | storedValue ++ x == "<p>" = "\n" ++ construct xs "" final

    | storedValue ++ x == "</p" = construct xs "</p" final
    | storedValue ++ x == "</p>" = "\n" ++ construct xs "" final

    -- body
    | storedValue ++ x == "<b" = construct xs "<b" final
    | storedValue ++ x == "<bo" = construct xs "<bo" final
    | storedValue ++ x == "<bod" = construct xs "<bod" final
    | storedValue ++ x == "<body" = construct xs "<body" final
    | storedValue ++ x == "<body>" = construct xs "" final

    | storedValue ++ x == "</b" = construct xs "</b" final
    | storedValue ++ x == "</bo" = construct xs "</bo" final
    | storedValue ++ x == "</bod" = construct xs "</bod" final
    | storedValue ++ x == "</body" = construct xs "</body" final
    | storedValue ++ x == "</body>" = construct xs "" final

    -- head
    | storedValue ++ x == "<h" = construct xs "<h" final
    | storedValue ++ x == "<he" = construct xs "<he" final
    | storedValue ++ x == "<hea" = construct xs "<hea" final
    | storedValue ++ x == "<head" = construct xs "<head" final
    | storedValue ++ x == "<head>" = construct xs "" final

    | storedValue ++ x == "</h" = construct xs "</h" final
    | storedValue ++ x == "</he" = construct xs "</he" final
    | storedValue ++ x == "</hea" = construct xs "</hea" final
    | storedValue ++ x == "</head" = construct xs "</head" final
    | storedValue ++ x == "</head>" = construct xs "" final

    -- html
    | storedValue ++ x == "<h" = construct xs "<h" final
    | storedValue ++ x == "<ht" = construct xs "<ht" final
    | storedValue ++ x == "<htm" = construct xs "<htm" final
    | storedValue ++ x == "<html" = construct xs "<html" final
    | storedValue ++ x == "<html>" = construct xs "" final

    | storedValue ++ x == "</h" = construct xs "</h" final
    | storedValue ++ x == "</ht" = construct xs "</ht" final
    | storedValue ++ x == "</htm" = construct xs "</htm" final
    | storedValue ++ x == "</html" = construct xs "</html" final
    | storedValue ++ x == "</html>" = construct xs "" final

    | otherwise =  x ++ construct xs "" final

--  construct ["a", "b", "<", "b", ">", "n", "m", "<", "/", "b", ">", " and there are", " ", "\n", " many", "<", "i", ">", "many", "<", "/", "i", ">", "<", "em", ">", "<", "/", "em", ">"] "" ""
