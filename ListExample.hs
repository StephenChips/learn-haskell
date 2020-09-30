import Data.List
import Debug.Trace

main = do
    _ <- print $ concat [[1,4,5],[1,3,4],[1,1]]
    
    -- concatMap :: first map each value to a list, then concatenate
    -- them
    _ <- print $ concatMap (replicate 3) [1..10]
    _ <- print $ and [True, False] -- False
    _ <- print $ or [True, False] -- True
    
    -- iterate :: Given a value, accumulate it, and put the result
    -- to the list.
    _ <- print $ take 10 $ iterate not True -- iterate
    _ <- print $ takeWhile (<0) [ x^2-9 | x<-[0..10] ]
    _ <- print $ dropWhile (<0) [ x^2-9 | x<-[0..10] ]
    _ <- print $ span (<0) [ x^2-9 | x<-[0..10] ]
    _ <- print $ sort [8,3,5,2,1,6,4,1]
    _ <- print $ find (==4) [1,4,5,6] -- reutrns `Just 4`
    _ <- print $ find (==4) [] -- returns `Nothing`
    _ <- print $ 4 `elemIndex` [1,3,3,4] -- returns `Just 3`
    _ <- print $ 10 `elemIndex` [] -- returns Nothing
    _ <- print $ ' ' `elemIndices` "Where are you?" -- returns [5,9]
    
    -- the `group` function groups same adjacent element into a list.
    _ <- print $ group [ 1,2,2,3,3,3,4,4,4,4 ] -- returns [[1], [2,2], [3,3,3], [4,4,4,4]]
    _ <- print $ group [ 1,3,3,1,1,3 ] -- returns [[1], [3,3], [1,1], [3]]
    _ <- print $ group $ concatMap (\x -> replicate x x) [1..4] -- returns same as above

    -- the `groupBy` function has its own critera.
    -- returns [[1],[2,2],[3,3,3],[4,4,4,4]]
    _ <- print $ groupBy (\x y -> odd x && odd y || even x && even y) [ 1,2,2,3,3,3,4,4,4,4 ]

    -- Like zip and zipWith, but works with three lists.
    _ <- print $ zip3 [1,2,3] [4,5,6] [7,8,9] -- returns [(1,4,7),(2,5,8),(3,6,9)]
    
    -- returns same as above
    _ <- print $ zipWith3 (\x y z -> (x, y, z)) [1,2,3] [4,5,6] [7,8,9]

    -- roughly equals str => map slice(n, len(str)) where n <- [0..len(str)]
    -- [[],[1],[1,3],[1,3,4],[1,3,4,5]]
    _ <- print $ inits [1,3,4,5]
    -- like above
    -- [[1,3,4,5],[3,4,5],[4,5],[5],[]]
    _ <- print $ tails [1,3,4,5]

    _ <- print $ "Hello" `isPrefixOf` "Hello, world" -- returns true
    _ <- print $ "world" `isSuffixOf` "Hello, world" -- return true
    _ <- print $ "o, wo" `isInfixOf` "Hello, world" -- reutrns true, BF search， O(n*m) the worst case
    _ <- print $ 3 `elem` [1,2,3,4] -- True
    _ <- print $ 3 `notElem` [1,2,3,4] -- False

    -- The essential part of QuickSort
    _ <- print $ partition (>3) [1,2,3,3,4,5,6]
    
    -- Let's define a QuickSort and use it (see following)
    _ <- print $ qsort [1,3,2,5,-1,3]

    -- There are zip4, zip5... (up to 7) to zip lists into one.
    -- and they all have the correspondent zipWith functions.

    -- Take a string, and return every line of that string.
    _ <- print $ lines "We all live\nin a yellow\nsubmarine.";
    
    -- The "reverse" function if `lines`
    _ <- print $ unlines [ "We all live", "in a yellow", "submarine."];

    -- splitting a string into words.
    _ <- print $ words "We all live in a yellow submarine.\nYellow submarine.\nYellow submarine";
    
    -- Similarily, it is the reverse function of the function `words`
    _ <- print $ unwords [ "Hey", "Jude." ]

    -- Find the element in the list, and filter it out.
    _ <- print $ delete 'h' "hoist" -- "oist"
    _ <- print $ delete "hello" [ "world", "hello" ]

    -- List difference function.
    -- Following expression will return [2,2,3],
    -- For there are two "1" and a "3" in the second list,
    -- two "1" and a "3" will be deleted in the first list.
    _ <- print $ concatMap (replicate 2) [1..3] \\ [1,1,3]
    
    -- Returns [1,1,2,2,3,3]
    _ <- print $ concatMap (replicate 2) [1..3] `intersect` [-5..3]

    -- Returns [1,1,2,2,3,3,-5,-4,-3,-2,-1,0]
    _ <- print $ concatMap (replicate 2) [1..3] `union` [-5..3]

    -- `nub` is the `uniq` function in other language, it has a bad name.
    _ <- print $ nub $ concatMap (replicate 2) [1..3]

    -- function length, take, drop, split, !! all has the generic version,
    -- that is able to take more generic argument.

    -- The `take` version accept `Int` for the 1st argument.
    _ <- print $ take 10 [1..]
    -- While `genericTake` version accept Typeclass `Integral`.
    _ <- print $ genericTake (10 :: Integer) [1..]
    
    print "DONE"

-- Just a dummy version of "QuickSort", which should be called as "Slow Sort"!
qsort :: (Ord a, Show a) => [a] -> [a]
qsort [] = []
qsort list = (qsort leftPart) ++ [pivot] ++ (qsort rightPart) where
    pivot:xs = list
    (leftPart, rightPart) = partition (<= pivot) xs;

