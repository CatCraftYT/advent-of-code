import Data.List (sort)
import Data.List.Extra (splitOn)

calcDistance :: Int -> Int -> Int
calcDistance a b = abs $ a - b

sortLists :: [Int] -> [Int] -> [(Int,Int)]
sortLists l1 l2 = zip (sort l1) (sort l2)

sumDists :: [(Int,Int)] -> Int
sumDists pairs = sum [calcDistance a b | (a,b) <- pairs]

parseLine :: [String] -> (Int,Int)
parseLine [s1,s2] = (read s1, read s2)

main = do
    contents <- readFile "inputs/day1.txt"
    let (l1,l2) = unzip [parseLine $ splitOn "   " s | s <- lines contents]
    print $ sumDists $ sortLists l1 l2
