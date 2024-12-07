import Data.List.Split ( splitOn )
import Control.Monad ( replicateM )
import Data.Bifunctor (second)

concatenate :: Integer -> Integer -> Integer
concatenate x y = read $ sx ++ sy
    where sx = show x 
          sy = show y

-- Honestly not entirely sure how this works (monads are magic)
createOperators :: Int -> [[Integer -> Integer -> Integer]]
createOperators n = map ((+) :) (replicateM (n - 1) [(*), (+), flip concatenate])

createPartialList :: [Integer] -> [[Integer -> Integer]]
createPartialList ns = [zipWith id ops ns | ops <- createOperators $ length ns]

foldPartials :: [[Integer -> Integer]] -> [Integer]
foldPartials = map $ foldl (\acc x -> x acc) 0

parseLines :: [String] -> [(Integer, [Integer])]
parseLines strs = [parseLine s | s <- strs]
    where parseLine s = (read.head $ split, map read (splitOn " " . tail.last $ split))
            where split = splitOn ":" s

getResults :: [(Integer, [Integer])] -> [(Integer, [Integer])]
getResults = map (second (foldPartials . createPartialList))

main = do
    contents <- parseLines . lines <$> readFile "inputs/day7.txt"
    print $ sum . map fst . filter (uncurry elem) $ getResults contents
