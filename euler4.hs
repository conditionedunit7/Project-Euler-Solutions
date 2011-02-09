isPalindrome :: (Show a) => a -> Bool
isPalindrome x = (reverse (show x)) == show x

palindromeProduct :: (Integral a) => a -> a
-- Find the largest palindrome that's a product of two numbers less than x
-- Would be faster to just have to get the head of a list, but this algorithm
-- doesn't seem to guarantee having the max at the head. Worth another look.
palindromeProduct x = maximum (filter isPalindrome products)
    where products = [y * z | y <- productRange, z <- productRange, z >= y]
          productRange = [(x-1),(x-2)..1]
