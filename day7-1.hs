import Data.List.Extra ( splitOn )
import Control.Monad ( replicateM )
import Data.Bifunctor (second)

-- Honestly not entirely sure how this works (monads are magic)
createOperators :: Num a => Int -> [[a -> a -> a]]
createOperators n = map ((+) :) (replicateM (n - 1) [(*), (+)])

createPartialList :: Num a => [a] -> [[a -> a]]
createPartialList ns = [zipWith id ops ns | ops <- createOperators $ length ns]

foldPartials :: Num a => [[a -> a]] -> [a]
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
