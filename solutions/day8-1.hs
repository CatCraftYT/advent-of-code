module Main where
import Data.List
import Data.Graph

type Point = (Int, Int, Int)

distance :: Point -> Point -> Float
distance (x1,y1,z1) (x2,y2,z2) = sqrt (a**2 + b**2 + c**2)
  where a = fromIntegral (x2-x1) :: Float
        b = fromIntegral (y2-y1) :: Float
        c = fromIntegral (z2-z1) :: Float

parseInput :: [String] -> [Point]
parseInput = map pointToString
  where pointToString s = (read x, read y, read z)
          where (x, postx) = break (==',') s
                (y, posty) = break (==',') (tail postx)
                z = tail posty

pairsToEdges :: Eq a => [(a, b)] -> [(Int, a, [b])]
pairsToEdges [] = []
pairsToEdges pairs = (0, point, linkedPoints) : pairsToEdges next
  where point = fst (head pairs)
        (linkedPoints, next) = foldl
          (\(linked, unlinked) p@(a,b) -> if a == point
                                          then (b : linked, unlinked)
                                          else (linked, p : unlinked))
          ([], []) pairs

-- Count the number of vertices in the 3 largest connected components
-- of the graph formed from edges between the n smallest distances
solve :: [Point] -> Int
solve points = product . take 3 . sortBy (flip compare) . map length . components $ graph
  where closestN = 1000 * 2   -- Multiply by 2 to account for duplicates
        closest = take closestN . sortOn (uncurry distance) . concat $ [[(a, b) | a <- points, a /= b] | b <- points]
        (graph, _, _) = graphFromEdges (pairsToEdges closest)

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day8.txt"
  -- print (parseInput contents)
  print (solve . parseInput $ contents)
