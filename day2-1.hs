import Data.List.Extra (splitOn)

isIncreasing :: [Int] -> Bool
isIncreasing [x,y] = x > y
isIncreasing (x:y:ys) = (x > y) && isIncreasing (y:ys)

isDecreasing :: [Int] -> Bool
isDecreasing [x,y] = x < y
isDecreasing (x:y:ys) = (x < y) && isDecreasing (y:ys)

isMonotonic :: [Int] -> Bool
isMonotonic xs = isIncreasing xs || isDecreasing xs

isChangingSlowly :: [Int] -> Bool
isChangingSlowly [x,y] = pred
    where diff = abs (x - y)
          pred = diff <= 3 && diff >= 1
isChangingSlowly (x:y:ys) = pred && isChangingSlowly (y:ys)
    where diff = abs (x - y)
          pred = diff <= 3 && diff >= 1

isSafe :: [Int] -> Bool
isSafe xs = isChangingSlowly xs && isMonotonic xs

parseLine :: String -> [Int]
parseLine xs = map read $ splitOn " " xs

main = do
    contents <- readFile "inputs/day2.txt"
    print.length $ filter id [isSafe.parseLine $ line | line <- lines contents]
