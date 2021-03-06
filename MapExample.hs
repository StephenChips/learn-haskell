import qualified Data.Map as Map

countChars :: String -> Map.Map Char Int
countChars str = Map.fromListWith (+) list where
    list = zip str (replicate (length str) 1)
    
heights = Map.fromList [("Jean", 165), ("Mary", 167), ("Scott", 170)]

howTallIs name =
    case (Map.lookup name heights) of
        Nothing -> "We don't know him. Who is he?"
        Just x -> name ++ " is " ++ show x ++ "cm tall."

main = do
    _ <- print $ howTallIs "Jean"
    _ <- print $ howTallIs "Bill"
    print "DONE"
