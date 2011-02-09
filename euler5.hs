leastMultiple :: (Integral a) => a -> a
-- Return the smallest number that is evenly divisible by the first x
-- natural numbers
leastMultiple x = head [y | y <- filter even [x, x*2..], length (filter (hasRemainder y) divisors) == 0]
    where divisors = [1..x]

hasRemainder :: (Integral a) => a -> a -> Bool
hasRemainder x y = (x `rem` y) /= 0