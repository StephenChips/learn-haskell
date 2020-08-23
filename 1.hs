cmp :: (Num a, Ord a) => a -> Ordering  
cmp a
    | a > 1    = GT  
    | a == 1   = EQ  
    | otherwise = LT 


search :: (Eq a) => a -> [a] -> Integer
search target ls = search' 0 ls where
    search' count [] = (-1)
    search' count (i:xs)
        | i == target = count
        | otherwise = search' (count + 1) xs
