import System.IO

main = do
    handle <- openFile "Texts/girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
