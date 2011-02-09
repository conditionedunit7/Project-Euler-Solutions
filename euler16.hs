powerOfTwo x = product (replicate x 2)

-- This reverses the order of the digits it separates
separateDigits x
    | x < 10 = [x]
    | otherwise = x `mod` 10 : separateDigits (x `div` 10)