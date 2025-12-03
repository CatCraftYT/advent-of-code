module Main where
import Data.Bifunctor
import Data.Maybe (isJust)
import Data.List (stripPrefix)

parseIDs :: String -> [(Int, Int)]
parseIDs [] = []
parseIDs s = getRange rangeString : parseIDs remainder
  where getRange = bimap read (read . tail) . break (=='-')
        (rangeString, remainder) = second (dropWhile (==',')) . break (==',') $ s

-- For every number in every range, for every way to split the number in two,
-- check whether if we repeatedly remove the first part of the split,
-- we end up with the empty list. For example:
-- 
-- For the split ("123", "123123123"), we can repeatedly remove 123
-- from the front of the second half and end up with the empty list.
-- For ("12", "3123123"), we cannot remove 12 from the start of that list,
-- so the second part is not a repeating sequence of the first part.
-- 
-- This solution is interesting in that I find it simple and elegant,
-- and yet somehow also very nasty.
solve :: [(Int, Int)] -> Int
solve = sum . map solveRange
  where solveRange (a, b) = sum . filter (isInvalid . show) $ [a..b]
        isInvalid s = any (isRepeatingSequence . (`splitAt` s)) [1..length s - 1]
          where isRepeatingSequence (a,b) = elem (Just "") . takeWhile isJust . iterate (>>= stripPrefix a) $ Just b
main :: IO ()
main = do
  contents <- parseIDs <$> readFile "inputs/day2.txt"
  print (solve contents)
