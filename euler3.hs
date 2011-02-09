primeFactors :: (Integral a) => a -> [a]
--Return a list of the prime factors of the input
primeFactors x = checkFactors x (possibleFactors x)

--Works, but is slow.  Would probably be faster to compute primes & check them
checkFactors :: (Integral a) => a -> [a] -> [a]
checkFactors x y
    | null y = []
    | f `isFactorOf` x = if (isPrime f)
                            then f : (checkFactors x t)
                            else checkFactors x t
    | otherwise = checkFactors x t
        where f = (head y)
              t = [z | z <- (tail y), not (f `isFactorOf` z)]

isPrime :: (Integral a) => a -> Bool
isPrime x = null (take 1 [y | y <- possibleFactors x, y `isFactorOf` x])

isFactorOf x y = (y `rem` x) == 0

possibleFactors x 
    | even x = 2 : (filter odd [3..(x `quot` 2)])
    | otherwise = filter odd [3..((x `quot` 3) + 1)]
