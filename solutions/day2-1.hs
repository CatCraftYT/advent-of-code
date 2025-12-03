module Main where
import Data.Bifunctor

parseIDs :: String -> [(Int, Int)]
parseIDs [] = []
parseIDs s = getRange rangeString : parseIDs remainder
  where getRange = bimap read (read . tail) . break (=='-')
        (rangeString, remainder) = second (dropWhile (==',')) . break (==',') $ s

-- For every number in every range, for every way to split the number in two,
-- check if each half is equal. For example,
-- [("1","23123"), ("12", "3123"), ("123","123"), ("1231","23")]
-- is equal on the third combination, so it is invalid.
solve :: [(Int, Int)] -> Int
solve = sum . map solveRange
  where solveRange (a, b) = sum . filter (isInvalid . show) $ [a..b]
        isInvalid s = any (uncurry (==) . (`splitAt` s)) [0..length s]

main :: IO ()
main = do
  contents <- parseIDs <$> readFile "inputs/day2.txt"
  print (solve contents)
