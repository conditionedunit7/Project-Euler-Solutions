import Data.List

--Start at 3 to avoid any weirdness with finding divisors
amicableNumbers x = findAmicables [3..x]

--This function could technically return amicable numbers that aren't in the 
--original list, if a pair straddles the upper boundary. However the chances of 
--this are pretty small (And it produces the right answer for Euler 21).
findAmicables (x:xs)
    | xs == [] = []
    | ((divisorSum candidate) == x) && (x /= candidate) = [x,candidate] ++ (findAmicables (delete candidate xs))
    | otherwise = findAmicables xs
    where candidate = divisorSum x

divisorSum x = sum $ allDivisors x

allDivisors x = 1 : (sortFactors x [2..(x `div` 2)])

--Name is a bit misleading, factors will be in a weird sort of order
sortFactors x ns
    | ns == []       = []
    | n `isFactor` x = if (n == pair) then [n] else [n,pair] ++ (sortFactors x (tail (takeWhile (< pair) ns)))
    | otherwise      = sortFactors x (filter (notFactor n) ns)
    where n = head ns
          pair = x `div` n

isFactor x y = (y `mod` x) == 0
notFactor x y = not $ isFactor x y