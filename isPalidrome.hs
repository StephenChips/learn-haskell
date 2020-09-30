main = interact respondPalidromes

respondPalidromes :: String -> String
respondPalidromes = unlines . map (\xs ->
    if isPalidrome xs
    then "palidrome"
    else "not a palidrome") . lines

isPalidrome :: String -> Bool
isPalidrome xs = xs == reverse xs