import Data.List.Split (splitOn)

parseLine :: [String] -> (Int,Int)
parseLine [s1,s2] = (read s1, read s2)

count :: Int -> [Int] -> Int
count x xs = length $ filter (==x) xs

calcSimilarityScore :: [Int] -> [Int] -> Int
calcSimilarityScore l1 l2 = sum [n * count n l2 | n <- l1]

main = do
    contents <- readFile "inputs/day1.txt"
    let (l1,l2) = unzip [parseLine $ splitOn "   " s | s <- lines contents]
    print $ calcSimilarityScore l1 l2
