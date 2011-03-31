getDigits x y = drop ((length ystr) - x) ystr
    where ystr = show $ expSeries y

expSeries x
    | x == 1    = 1
    | otherwise = (x ^ x) + (expSeries (x - 1))