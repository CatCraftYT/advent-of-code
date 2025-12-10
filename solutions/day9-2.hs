module Main where
import Data.Bifunctor
import Data.List
import Data.Ord
import Control.Parallel.Strategies

type Vertex = (Int,Int)
type Edge = (Vertex, Vertex)
type Polygon = [Edge]

parseInput :: [String] -> [Vertex]
parseInput = map stringToVertex
  where stringToVertex = bimap read (read . tail) . break (==',')

createPolygon :: [Vertex] -> Polygon
createPolygon [] = []
createPolygon vertices = (last vertices, head vertices) : createPolygon' vertices
  where createPolygon' (v1:v2:vs) = (v1,v2) : createPolygon' (v2:vs)
        createPolygon' (_:_) = []
        createPolygon' [] = []

combinations :: [Vertex] -> [(Vertex,Vertex)]
combinations points = concat [[(a, b) | a <- points, a /= b && fst b >= fst a] | b <- points]

-- https://en.wikipedia.org/wiki/Point_in_polygon#Ray_casting_algorithm
-- https://web.archive.org/web/20130126163405/http://geomalgorithms.com/a03-_inclusion.html
insidePolygon :: Vertex -> Polygon -> Bool
insidePolygon (vx,vy) p =
  -- If the point is on any edge, it is considered inside the polygon
     any (\((x1,y1),(x2,y2)) -> (vy == y1 && vy == y2 && vx `betweenInclusive` (x1,x2))
                             || (vx == x1 && vx == x2 && vy `betweenInclusive` (y1,y2))) p
  -- For a raycast straight to the right,
  -- if the number of intersections with edges is odd, then it is inside
  || (odd . length $ filter intersectionTest p)
  where betweenExclusive x (a,b) = (x > a && x < b) || (x > b && x < a)
        betweenInclusive x (a,b) = (x >= a && x <= b) || (x >= b && x <= a)
        intersectionTest ((x1,y1),(x2,y2))
          | x1 < vx || x2 < vx = False
          | isHorizontalEdge   = False
          | isUpwardEdge       = (y1 == vy) || vy `betweenExclusive` (y1,y2)
          | isDownwardEdge     = (y2 == vy) || vy `betweenExclusive` (y1,y2)
          | otherwise          = error "Unexpected intersectionTest case"
          where isUpwardEdge = y2 > y1
                isDownwardEdge = y2 < y1
                isHorizontalEdge = y1 == y2

rectangleSize :: (Vertex,Vertex) -> Int
rectangleSize (a,b) = (abs (fst b - fst a) + 1) * (abs (snd b - snd a) + 1)

rectanglePoints :: (Vertex,Vertex) -> [Vertex]
rectanglePoints (v1@(x1,y1), v2@(x2,y2)) = [v1, (x1, y2), v2, (x2, y1)]

pointsAlongEdge :: Edge -> [Vertex]
pointsAlongEdge ((x1, y1), (x2, y2))
  | x1 == x2 = zip (repeat x1) [minY..maxY]
  | y1 == y2 = zip [minX..maxX] (repeat y1)
  | otherwise = error "Line must be horizontal or vertical"
  where maxX = max x1 x2
        maxY = max y1 y2
        minX = min x1 x2
        minY = min y1 y2

-- The given points form a simple polygon. For every pair of points, we
-- calculate the points along the perimeter of the rectangle formed from them,
-- then determine if all of the points along the perimeter are within the polygon.
-- If they are, then the rectangle itself is within the polygon,
-- so it is valid. Not sure if there is a name for this property, or if it
-- is actually true in general, but it seems to work.
-- Also, rather than computing all valid rectangles then picking the maximum,
-- find and order all the largest rectangles, then pick the first valid one.
solve :: [Vertex] -> Int
-- solve vertices = rectangleSize . process . sortOn (Data.Ord.Down . rectangleSize) . combinations $ vertices
solve vertices = rectangleSize . process . sortOn (Data.Ord.Down . rectangleSize) . combinations $ vertices
  where polygon = createPolygon vertices
        -- Yes, I know this looks dumb. Filter doesn't seem to work in parallel,
        -- and doing this *does* result in a significant speedup and CPU usage.
        -- We can't just use this on the entire combinations list, because
        -- when calculating a list in parallel it evaluates every element,
        -- negating the sorting optimization and massively slowing it down.
        process l
          | not . null $ processedBatch = head processedBatch
          | otherwise = process (drop batchSize l)
          where batchSize = 5192  -- 5192 seems to be optimal for me. Probably depends on CPU
                batch = take batchSize l
                processedBatch = map snd . filter fst . parMap rdeepseq (\x -> (validRectangle x, x)) $ batch
        -- Check corners first since they are furthest apart
        validRectangle r = all (`insidePolygon` polygon) corners && all (all (`insidePolygon` polygon) . pointsAlongEdge) (createPolygon corners)
          where corners = rectanglePoints r

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day9.txt"
  -- print (length . combinations . parseInput $ contents)
  putStrLn "NOTE: day9-1 should be run with arguments '+RTS -N' for multithreading"
  print (solve . parseInput $ contents)
