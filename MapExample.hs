import qualified Data.Map;

countChars :: String -> Map.Map Char Int
countChars str = Map.fromListWith (+) list where
    list = zip str (replicate (length str) 1)
