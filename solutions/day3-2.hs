module Main where
import Data.List

combinations :: String -> [String]
combinations = filter ((==12) . length) . subsequences

solve :: [String] -> Int
solve = sum . map (maximum . map read . combinations)

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day3-test.txt"
  print (solve contents)

