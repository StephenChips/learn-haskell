import Data.List

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

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:_) = Just x

extract :: a -> Maybe a -> a
extract defVal Nothing = defVal
extract defVal (Just a) = a

describeList :: [a] -> String
describeList xs = "The list " ++ what xs where 
    what []   = "is empty"
    what [x]  = "has single element"
    what _ = "has many elements";

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

sumList :: (Num a) => [a] -> a;
sumList ls = foldl (+) 0 ls 


fib :: (Integral p, Integral t) => p -> t
fib x = fib' 0 1 0 x where
    fib' a b cnt end
        | (cnt >= end) = b
        | otherwise = fib' b (a + b) (cnt + 1) x


search' :: (Eq a) => [a] -> [a] -> Bool
search' needle haystack =
    let
        isPostfix True _ = True
        isPostfix False x = needle `isPrefixOf` x
    in foldl' isPostfix False (tails haystack)
