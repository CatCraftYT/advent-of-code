module Main where
import Data.Maybe
import Data.List
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

mapToInput :: Map.Map Coordinates Bool -> [String]
mapToInput = transpose . map (map (toTextRepresentation . snd)) . groupBy (\((x1, _), _) ((x2, _), _) -> x1 == x2) . Map.toList
  where toTextRepresentation b
          | b = '@'
          | otherwise = '.'

-- Reusing the result of removableRolls would probably be more efficient
-- but this is easier to read and write.
solve :: Map.Map Coordinates Bool -> Int
solve = sum . map (length . removableRolls) . takeWhile (not . null . removableRolls) . iterate removeReachableRolls
  where removableRolls m = filter ((<4) . length . filter (fromMaybe False . (`Map.lookup` m)) . neighbours . fst) . filter snd $ Map.toList m
        removeRolls :: Map.Map Coordinates Bool -> [(Coordinates, Bool)] -> Map.Map Coordinates Bool
        removeRolls = foldr (\(pos, _) -> Map.insert pos False)
        removeReachableRolls rollMap = removeRolls rollMap (removableRolls rollMap)
        neighbours (x, y) = [(x+1,y), (x-1,y), (x,y+1), (x,y-1), (x+1,y+1), (x-1,y+1), (x+1,y-1), (x-1,y-1)]

main :: IO ()
main = do
  contents <- parseInput . lines <$> readFile "inputs/day4.txt"
  print (solve contents)
  -- putStrLn (concatMap ((++"\n\n") . intercalate "\n" . mapToInput) . solve $ contents)
