module Main where
import Data.Bifunctor

mergeRanges :: [(Integer,Integer)] -> [(Integer,Integer)]
mergeRanges [] = []
mergeRanges ranges
  | ranges == next = next
  | otherwise = mergeRanges next
  where next = mergeStep ranges
        mergeStep [] = []
        mergeStep (r:rs) = merged : mergeStep newRanges
          where newMin = foldl (\m (low, high) -> if isWithinRange high r && low < m then low else m) (fst r) rs
                newMax = foldl (\m (low, high) -> if isWithinRange low (newMin, snd r) && high > m then high else m) (snd r) rs
                merged = (newMin, newMax)
                newRanges = filter (\(a,b) -> not (isWithinRange a merged && isWithinRange b merged)) rs
                isWithinRange n (a,b) = n >= a && n <= b 

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
