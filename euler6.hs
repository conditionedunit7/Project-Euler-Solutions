squareSumDiff :: (Integral a) => a -> a
-- Computes the difference between the square of the sum
-- and the sum of the squares of the first x integers
squareSumDiff x = ((sum [1..x]) ^ 2) - (sum [x ^2 | x <- [1..x]])