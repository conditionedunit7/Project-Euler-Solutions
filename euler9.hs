triplet = [(a,b,c) | a <- [1..998], b <- [(a+1)..998], c <- [(b+1)..998], ((a + b + c) == 1000) && (pythagorean a b c)]
pythagorean x y z = ((x * x) + (y * y)) == (z * z)

--Quicker to calculate squares of all # 1-998, then compare?