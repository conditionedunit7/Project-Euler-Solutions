powerSum :: (Integral a) => a -> a -> a
powerSum pow limit = sum (filter (checkPower pow) [10..limit])

checkPower :: (Integral a) => a -> a -> Bool
checkPower pow n =
    if (sum (map (toN pow) digits) == n)
    then True
    else False
        where digits = separateDigits n

toN n x = x ^ n

-- This reverses the order of the digits it separates
separateDigits x
    | x < 10 = [x]
    | otherwise = x `mod` 10 : separateDigits (x `div` 10)
