import System.IO
import Data.List
import Data.Char

main = do
    withFile "euler22.txt" ReadMode (\handle -> do
        allnames <- hGetContents handle
        let names = read ("[" ++ allnames ++ "]")
            nameSum = foldl (\acc name -> acc + (findScore name)) 0 (zip [1..] (sort names))
        print nameSum)

findScore (pos,name) = pos * (foldl (\acc letter -> acc + (charVal letter)) 0 name)

-- 'A' will be 1, 'B' 2, etc. Not so great for lower-case.
charVal x = (ord x) - 64