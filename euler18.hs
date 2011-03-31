import System.IO
import Data.List
import Data.Char

main = do
    withFile "euler18.txt" ReadMode (\handle -> do
        numstr <- hGetContents handle
        let nums = parseNums numstr
            maxSum = findMax nums
        print maxSum)

parseNums :: String -> [[Int]]
parseNums s = map (map  ((strToInt 1) . reverse)) (map words $ lines s)
    where strToInt acc [] = 0
          strToInt acc (x:xs) = ((digitToInt x) * acc) + (strToInt (acc * 10) xs)

findMax (x:xs) = maximum $ map snd (findPaths [(0,(x !! 0))] xs)

-- The trimming in this function assumes 2-digit numbers (max 99)
findPaths ps (xs)
    | xs == []  = ps
    | otherwise = findPaths (concat [getNexts p | p <- ps, possibleMax p]) (tail xs)
    where getNexts (i,n) = [(i,(x !! i)+n),(i+1,(x !! (i+1))+n)]
          possibleMax (i,n) = (n + potentialMax) > currentMax
          x = head xs
          currentMax = maximum $ map snd ps
          potentialMax = (length xs) * 99