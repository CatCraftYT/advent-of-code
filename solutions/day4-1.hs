module Main where
import Data.Maybe
import qualified Data.Map.Strict as Map

type Coordinates = (Int,Int)

parseInput :: [String] -> Map.Map Coordinates Bool
parseInput input = Map.fromList mapList
  where mapList = concat $ foldl (\rows row ->
            foldl (\cols col -> ((length cols, length rows), isPaperRoll col) : cols) [] row : rows
          ) [] input
        isPaperRoll c
          | c == '@' = True
          | otherwise = False

solve :: Map.Map Coordinates Bool -> Int
solve m = length . filter ((<4) . length . filter (fromMaybe False . (`Map.lookup` m)) . neighbours . fst) . filter snd $ Map.toList m
  where neighbours (x, y) = [(x+1,y), (x-1,y), (x,y+1), (x,y-1), (x+1,y+1), (x-1,y+1), (x+1,y-1), (x-1,y-1)]

main :: IO ()
main = do
  contents <- parseInput . lines <$> readFile "inputs/day4.txt"
  print (solve contents)
