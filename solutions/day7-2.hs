module Main where
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map

type Coordinates = (Int,Int)
type TileMap = Map.Map Coordinates Tile
data Tile = Empty | Splitter | TachyonBeam Int deriving (Show, Eq)

isTachyonBeam :: Tile -> Bool
isTachyonBeam (TachyonBeam _) = True
isTachyonBeam _ = False

getNBeams :: Tile -> Int
getNBeams (TachyonBeam n) = n
getNBeams _ = 0

charToTile :: Char -> Tile
charToTile c
  | c == '.' = Empty
  | c == '^' = Splitter
  | c == '|' = TachyonBeam 1
  | c == 'S' = TachyonBeam 1
  | otherwise = error ("Parse error in charToTile: " ++ [c])

parseInput :: [String] -> TileMap
parseInput allRows = parseInput' allRows Map.empty 0
  where parseRow [] m _ _ = m
        parseRow (col:row) m x y = parseRow row (Map.insert (x,y) (charToTile col) m) (x+1) y
        parseInput' [] m _ = m
        parseInput' (row:rows) m depth = parseInput' rows (parseRow row m 0 depth) (depth + 1)

mapToInput :: TileMap -> [String]
mapToInput = transpose . map (map (tileToChar . snd)) . groupBy (\((x1, _),_) ((x2,_),_) -> x1 == x2) . Map.toList
  where tileToChar (TachyonBeam n) = last (show n)
        tileToChar t
          | t == Empty = '.'
          | t == Splitter = '^'
          | otherwise = error "Invalid tile"

-- Simulate row by row to the end instead of one step over the entire grid,
-- otherwise we end up counting the same beam over and over
simulate :: TileMap -> Int -> TileMap
simulate initialMap nRows = fst . (!! nRows) . iterate (\(m, n) -> (simulateStep m n, n+1)) $ (initialMap, 0 :: Int)
  where simulateStep stepMap row = foldr moveDown stepMap beams
          where beams = map fst . filter ((==row) . snd . fst) . filter (isTachyonBeam . snd) . Map.toList $ stepMap
                moveDown (x,y) m
                  | isNothing maybeBelow = m
                  | below == Empty = Map.insert (x,y+1) here m
                  | below == Splitter = useSplitter
                  | isTachyonBeam below = Map.insertWith combineBeam (x,y+1) here m
                  | otherwise = error ("Unexpected tile " ++ show below)
                  where here = m Map.! (x,y)
                        maybeBelow = Map.lookup (x,y+1) m
                        below = fromJust maybeBelow
                        useSplitter = Map.insertWith combineBeam (x-1, y+1) here . Map.insertWith combineBeam (x+1, y+1) here $ m
                        combineBeam (TachyonBeam n1) (TachyonBeam n2) = TachyonBeam (n1+n2)
                        combineBeam (TachyonBeam n) _ = TachyonBeam n
                        combineBeam _ (TachyonBeam n) = TachyonBeam n
                        combineBeam _ _ = error "Trying to create new beam"

-- Simulate the beam, and keep track of the number of beams
-- that pass through each position. The total number of beams
-- that reach the final row is the number of possible paths.
solve :: [String] -> Int
solve input = sum . map getNBeams . finalRow $ simulate tileMap height
  where height = length input - 1
        tileMap = parseInput input
        finalRow = map snd . filter ((==height) . snd . fst) . Map.toList

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day7.txt"
  -- putStrLn (intercalate "\n" . mapToInput . flip simulate (length contents - 1) . parseInput $ contents)
  print (solve contents)
