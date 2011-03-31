maxChain :: (Integral a) => a -> a
maxChain n = fst (foldl1 (\acc x -> if snd(x) > snd(acc) then x else acc) $ chains n)

chains :: (Integral a) => a -> [(a,Int)]
chains n = [(x,length (collatz x)) | x <- [2..n]]

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz x
    | even x = x : collatz (x `div` 2)
    | odd x  = x : collatz (x * 3 + 1)