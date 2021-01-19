module IO where

import Control.Monad
import Data.Char

askColors = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number" ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1,2,3,4 are: "
    mapM (\a -> do
            putStr a
            putChar ' '   
        ) colors

upperStdin = do
    contents <- getContents
    putStr (map toUpper contents)

main = upperStdin