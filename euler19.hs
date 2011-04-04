import Data.List

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

week = [minBound .. maxBound] :: [Day]
jan = [1..31]
febReg = [1..28]
febLeap = [1..29]
mar = [1..31]
apr = [1..30]
may = [1..31]
jun = [1..30]
jul = [1..31]
aug = [1..31]
sep = [1..30]
oct = [1..31]
nov = [1..30]
dec = [1..31]
yearReg = concat [jan, febReg, mar, apr, may, jun, jul, aug, sep, oct, nov, dec]
yearLeap = concat [jan, febLeap, mar, apr, may, jun, jul, aug, sep, oct, nov, dec]

countFirsts start stop d = foldl (\acc x -> if x == (1,Sunday) then acc + 1 else acc) 0 (concat (buildYears start stop d))

-- unfold might be a bit more elegant but it's way ugly to look at
buildYears year stop d
    | year > stop = []
    | otherwise   = thisYear : (buildYears (year + 1) stop (newDay))
        where thisYear = makeYear year d
              newDay = nextDay (snd (last thisYear))

nextDay :: Day -> Day
nextDay d = if d == (maxBound::Day) then (minBound::Day) else succ d
                      
makeYear :: Int -> Day -> [(Int,Day)]
-- Given a year and the first day of the year, make a list of day of month/
-- day of week pairs
makeYear x d
    | isLeap x  = zip yearLeap (dropWhile (< d) (cycle week))
    | otherwise = zip yearReg (dropWhile (< d) (cycle week))

isLeap x = (((x `rem` 4) == 0) && ((x `rem` 100) /= 0)) || ((x `rem` 400) == 0)
