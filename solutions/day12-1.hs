module Main where
import Data.Bifunctor

newtype Shape = Shape { tiles :: [[Bool]] } deriving Show
data Region = Region { size :: (Int, Int), requirements :: [(Shape, Int)] } deriving Show

-- Taken from day 6 (again)
splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s
  | null post = [pre]
  | otherwise = pre : splitOn c (tail post)
  where (pre, post) = break (==c) s

-- Assuming the shapes are all 3x3
parseInput :: [String] -> [Region]
parseInput input = regions
  where (shapeSection, regionStrings) = break ('x' `elem`) input
        shapeStrings = takeWhile (not . null) . map (take 3) . iterate (drop 3) . filter (all (`elem` ['#','.'])) . filter (not . null) $ shapeSection
        shapes = map parseShape shapeStrings
        regions = map parseRegion regionStrings
        parseShape = Shape . map (map (=='#'))
        parseRequirements = filter ((>0) . snd) . zip shapes . map read . splitOn ' '
        parseRegion = uncurry Region . bimap (bimap read (read . tail) . break (=='x')) (parseRequirements . drop 2) . break (==':')

-- Filter out regions which could never possibly fit the shapes, because
-- the number of total tiles given by the requirements exceeds the number
-- of free tiles available in the region.
filterValidRegions :: [Region] -> [Region]
filterValidRegions = filter isValid
  where isValid r = (<=regionArea r) . sum . map (\(shape, n) -> (*n) . length . filter id . concat . tiles $ shape) . requirements $ r
        regionArea = uncurry (*) . size

-- Apparently the inputs for this day are all determined by
-- the basic validity test (no packing algos are needed).
solve :: [String] -> Int
solve = length . filterValidRegions . parseInput

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day12.txt"
  -- print (parseInput contents)
  print (solve contents)
