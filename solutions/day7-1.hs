module Main where
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map

type Coordinates = (Int,Int)
type TileMap = Map.Map Coordinates Tile
data Tile = Empty | Splitter | UsedSplitter | TachyonBeam deriving (Show, Eq)

charToTile :: Char -> Tile
charToTile c
  | c == '.' = Empty
  | c == '^' = Splitter
  | c == '|' = TachyonBeam
  | c == 'S' = TachyonBeam
  | otherwise = error ("Parse error in charToTile: " ++ [c])

parseInput :: [String] -> TileMap
parseInput allRows = parseInput' allRows Map.empty 0
  where parseRow [] m _ _ = m
        parseRow (col:row) m x y = parseRow row (Map.insert (x,y) (charToTile col) m) (x+1) y
        parseInput' [] m _ = m
        parseInput' (row:rows) m depth = parseInput' rows (parseRow row m 0 depth) (depth + 1)

mapToInput :: TileMap -> [String]
mapToInput = transpose . map (map (tileToChar . snd)) . groupBy (\((x1, _),_) ((x2,_),_) -> x1 == x2) . Map.toList
  where tileToChar t
          | t == Empty = '.'
          | t == Splitter = '^'
          | t == UsedSplitter = 'v'
          | t == TachyonBeam = '|'
          | otherwise = error "Invalid tile"

simulate :: TileMap -> TileMap
simulate initialMap = foldr moveDown initialMap beams
  where beams = map fst . filter ((==TachyonBeam) . snd) . Map.toList $ initialMap
        moveDown (x,y) m
          | isNothing maybeBelow = m
          | below == Empty = Map.insert (x,y+1) TachyonBeam m
          | below == Splitter = useSplitter
          | below == UsedSplitter = useSplitter
          | below == TachyonBeam = m
          | otherwise = error ("Unexpected tile " ++ show below)
          where maybeBelow = Map.lookup (x,y+1) m
                below = fromJust maybeBelow
                useSplitter = Map.insert (x, y+1) UsedSplitter . Map.insert (x-1, y+1) TachyonBeam . Map.insert (x+1, y+1) TachyonBeam $ m

solve :: [String] -> Int
solve input = length . filter ((==UsedSplitter) . snd) . Map.toList . last . takeWhile (notElem TachyonBeam . finalRow) . iterate simulate $ tileMap
  where height = length input - 1
        tileMap = parseInput input
        finalRow = map snd . filter ((==height) . snd . fst) . Map.toList

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day7.txt"
  -- putStrLn (intercalate "\n" . mapToInput . last . take 100 . iterate simulate . parseInput $ contents)
  print (solve contents)
