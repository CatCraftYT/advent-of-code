module Main where
import Data.Bifunctor

type Point = (Integer,Integer)

parseInput :: [String] -> [Point]
parseInput = map stringToPoint
  where stringToPoint = bimap read (read . tail) . break (==',')

combinations :: [Point] -> [(Point,Point)]
combinations points = concat [[(a, b) | a <- points, a /= b && fst b >= fst a] | b <- points]

rectangleSize :: (Point,Point) -> Integer
rectangleSize (a,b) = (abs (fst b - fst a) + 1) * (abs (snd b - snd a) + 1)

solve :: [Point] -> Integer
solve = maximum . map rectangleSize . combinations

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day9.txt"
  -- print (combinations . parseInput $ contents)
  print (solve . parseInput $ contents)
