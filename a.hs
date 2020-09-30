spinWords = words . (map (\str -> 
    if length str >= 5
    then reverse str
    else str))

