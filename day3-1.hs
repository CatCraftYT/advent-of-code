import Text.Regex.TDFA

getMulPairs :: String -> [[String]]
getMulPairs s = map tail $ s =~ "mul\\(([0-9]+),([0-9]+)\\)"

sumMulPairs :: [[String]] -> Int
sumMulPairs xs = sum [read (head x) * read (last x) | x <- xs]

main = do
    contents <- readFile "inputs/day3.txt"
    print $ sumMulPairs.getMulPairs $ contents
