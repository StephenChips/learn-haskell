cmp :: (Num a, Ord a) => a -> Ordering  
cmp a
    | a > 1    = GT  
    | a == 1   = EQ  
    | otherwise = LT 