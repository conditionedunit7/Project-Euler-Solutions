firstTriangle x = head (dropWhile ((< x) . snd) triangleDivisors)

triangleDivisors = map numLengthTuple triangleNums

numLengthTuple x = (x,(length $ allFactors x))

triangleNums = scanl1 (\acc x -> acc + x) [1..]

--Too slow to be practical for larger numbers
--allFactors :: (Integral a) => a -> [a]
--allFactors x = x : [n | n <- [half,(half - 1)..1], (x `mod` n) == 0]
--    where half = x `div` 2

allFactors x = [1,x] ++ (sortFactors x [2..(x `div` 2)])

--Name is a bit misleading, factors will be in a weird sort of order
sortFactors x ns
    | ns == []       = []
    | n `isFactor` x = if (n == pair) then [n] else [n,pair] ++ (sortFactors x (tail (takeWhile (< pair) ns)))
    | otherwise      = sortFactors x (filter (notFactor n) ns)
    where n = head ns
          pair = x `div` n

isFactor x y = (y `mod` x) == 0
notFactor x y = not $ isFactor x y