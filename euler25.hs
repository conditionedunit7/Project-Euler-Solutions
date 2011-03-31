fiboLength x = fst $ head (dropWhile ((< x) . intLength . snd) (map fiboTuple [1..]))

fiboTuple x = (x,head $ fibonacci x)

fibonacci :: (Integral a) => a -> [a]
-- Returns the Fibonacci sequence (In reverse) out to the given number of terms
fibonacci 0 = []
fibonacci 1 = [1]
fibonacci 2 = [1,1]
fibonacci x = sum (take 2 fibox) : fibox
    where fibox = fibonacci (x-1)

intLength x
    | x < 10 = 1
    | otherwise = 1 + intLength (x `div` 10)