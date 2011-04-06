import Data.List

spiralDiags :: (Integral a) => a -> a
spiralDiags x = sum ((getDiags spi) `union` (getDiags (reverse spi)))
    where spi = getSpiral x

getDiags :: (Integral a) => [[a]] -> [a]
getDiags xs = getDiag 0 xs

getDiag :: (Integral a) => Int -> [[a]] -> [a]
getDiag n (x:xs)
    | n > (length x) = []
    | xs == []       = [x !! n]
    | otherwise      = (x !! n):(getDiag (n + 1) xs)

getSpiral :: (Integral a) => a -> [[a]]
getSpiral len = buildSpiral [[1]] [2..(len * len)]

buildSpiral :: (Integral a) => [[a]] -> [a] -> [[a]]
--Given a matrix and a list of numbers, add those numbers to the matrix in a 
--spiral pattern, going counterclockwise. This can be converted to a clockwise-
--building matrix as specified by the problem by doing a map of the reverse 
--method, but it's not necessary since the diagonals of odd-sided matrices will 
--be the same. The matrix won't necessarily be square if the list of numbers is 
--the wrong length.
buildSpiral spi ns
    | ns == []  = spi
    | otherwise = buildSpiral ((reverse (take len ns)):nextSpi) (drop len ns)
        where nextSpi = (transpose . reverse) spi
              len = (length . head) nextSpi