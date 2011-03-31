import System.IO
import Data.List
import Data.Char

-- There are definitely some better ways to do all this. Submatrices? Identity multiplication?
main = do
    withFile "euler11.txt" ReadMode (\handle -> do
        numstr <- hGetContents handle
        let nums = parseNums numstr
            maxproduct = maximum (map product ((findDiags 4 nums) ++ (findVert 4 nums) ++ (findHoriz 4 nums)))
        print maxproduct)

parseNums :: String -> [[Int]]
parseNums s = map (map  ((strToInt 1) . reverse)) (map words $ lines s)
    where strToInt acc [] = 0
          strToInt acc (x:xs) = ((digitToInt x) * acc) + (strToInt (acc * 10) xs)

findDiags :: (Integral a) => Int -> [[a]] -> [[a]]
findDiags n xs = (findDiag n xs) ++ findDiag n (reverse xs)

findDiag :: (Integral a) => Int -> [[a]] -> [[a]]
-- This assumes that the sub-lists are of the same length
findDiag n xs
    | ((length xs) < n) || ((length (head xs)) < n) = []
    | otherwise = (transpose [(drop z (take ((length (xs !! z)) - ((n - 1) - z)) (xs !! z))) | z <- [0..(n - 1)]]) ++ (findDiag n (tail xs))

findVert :: (Integral a) => Int -> [[a]] -> [[a]]
findVert n xs = findHoriz n $ transpose xs

findHoriz :: (Integral a) => Int -> [[a]] -> [[a]]
findHoriz n (x:xs)
    | xs == [] = findHorizRow n x
    | otherwise = (findHorizRow n x) ++ (findHoriz n xs)

findHorizRow :: (Integral a) => Int -> [a] -> [[a]]
findHorizRow n xs
    | length xs < n = []
    | otherwise = (take n xs) : (findHorizRow n (tail xs))