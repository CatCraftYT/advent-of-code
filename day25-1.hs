import Data.List.Extra ( isPrefixOf, transpose, splitOn )

type Lock = [Int]
type Key = [Int]

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count s (c:cs) = (if c == s then 1 else 0) + count s cs

parseLocks :: [[String]] -> ([Lock], [Key])
parseLocks s = (lockHeights,keyHeights)
    where locks = filter (isFilled.head) s
          keys = filter (isFilled.last) s
          lockHeights = toHeights locks
          keyHeights = toHeights keys
          toHeights = map (map (subtract 1 . count '#') . transpose)
          isFilled l = l `isPrefixOf` repeat '#'

sumLists :: Num a => [a] -> [a] -> [a]
sumLists a b
    | length a /= length b = error "Lists of differing length"
    | otherwise = zipWith (+) a b

lockFits :: Lock -> Key -> Bool
lockFits l k = not.any (>5) $ sumLists l k

solve :: [Lock] -> [Key] -> Int
solve ls ks = count True . concatMap (\l -> map (lockFits l) ks) $ ls

main = do
    contents <- map lines . splitOn "\n\n" <$> readFile "inputs/day25.txt"
    print $ uncurry solve . parseLocks $ contents
