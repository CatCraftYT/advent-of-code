module Main where
import Data.Bifunctor

solve :: [String] -> [String] -> Int
solve rangeStrings idStrings = length . filter (\i -> any (isWithinRange i) ranges) $ ids
  where ranges = map (bimap read (read . tail) . break (=='-')) rangeStrings
        ids = map read idStrings
        ranges :: [(Integer,Integer)]
        ids :: [Integer]
        isWithinRange n (a,b) = n >= a && n <= b 

main :: IO ()
main = do
  (ranges, ids) <- second tail . break null . lines <$> readFile "inputs/day5.txt"
  print (solve ranges ids)
