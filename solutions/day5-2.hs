module Main where
import Data.Bifunctor

isWithinRange :: Integer -> (Integer,Integer) -> Bool
isWithinRange n (a,b) = n >= a && n <= b 

mergeRanges :: [(Integer,Integer)] -> [(Integer,Integer)]
mergeRanges [] = []
mergeRanges ranges
  | ranges == next = filtered
  | otherwise = mergeRanges next
  where next = mergeStep ranges
        -- Remove all the remaining ranges which are within another range.
        -- This *should* be removed by the newRanges step... but it isn't...
        -- so just do this to fix it for now.
        filtered = filter (\(a,b) -> not $ any (\x -> (a,b) /= x && isWithinRange a x && isWithinRange b x) next) next

mergeStep :: [(Integer,Integer)] -> [(Integer,Integer)]
mergeStep [] = []
mergeStep (r:rs) = merged : mergeStep newRanges
  where newMin = foldl (\m (low, high) -> if isWithinRange high r && low < m then low else m) (fst r) rs
        newMax = foldl (\m (low, high) -> if isWithinRange low (newMin, snd r) && high > m then high else m) (snd r) rs
        merged = (newMin, newMax)
        newRanges = filter (\(a,b) -> not (isWithinRange a merged && isWithinRange b merged)) rs

-- Add nRanges to account for endpoints
solve :: [String] -> Integer
solve rangeStrings = (+ nRanges) . sum . map (\(a,b) -> b-a) $ mergedRanges
  where ranges = map (bimap read (read . tail) . break (=='-')) rangeStrings
        mergedRanges = mergeRanges ranges
        nRanges = toInteger (length mergedRanges)
        ranges :: [(Integer,Integer)]

main :: IO ()
main = do
  ranges <- takeWhile (not . null) . lines <$> readFile "inputs/day5.txt"
  print (solve ranges)
