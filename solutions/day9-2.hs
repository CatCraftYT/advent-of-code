module Main where
import Data.Bifunctor

type Vertex = (Integer,Integer)
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

-- https://en.wikipedia.org/wiki/Point_in_polygon#Ray_casting_algorithm
-- We can make some simplifications because the lines have integer points,
-- and are also only perfectly horizontal or vertical.
insidePolygon :: Vertex -> Polygon -> Bool
insidePolygon (vx,vy) p
  -- If the point is on any edge, it is inside the polygon
  | any (\((x1,y1),(x2,y2)) -> vy == y1 && vy == y2 && vx `betweenInclusive` (x1,x2)) p = True
  | any (\((x1,y1),(x2,y2)) -> vx == x1 && vx == x2 && vy `betweenInclusive` (y1,y2)) p = True
  -- If the number of intersections with edges is odd, then it is inside
  | odd . length $ filter (\((x1,y1),(x2,y2)) -> (x1 > vx && x2 > vx) && vy `betweenExclusive` (y1,y2)) p = True
  | otherwise = False
  where betweenExclusive x (a,b) = (x > a && x < b) || (x > b && x < a)
        betweenInclusive x (a,b) = (x >= a && x <= b) || (x >= b && x <= a)

combinations :: [Vertex] -> [(Vertex,Vertex)]
combinations points = concat [[(a, b) | a <- points, a /= b && fst b >= fst a] | b <- points]

rectangleSize :: (Vertex,Vertex) -> Integer
rectangleSize (a,b) = (abs (fst b - fst a) + 1) * (abs (snd b - snd a) + 1)

rectanglePoints :: (Vertex,Vertex) -> [Vertex]
rectanglePoints (v1@(x1,y1), v2@(x2,y2)) = [v1, v2, (x1, y2), (x2, y1)]

-- The given points form a polygon. For every pair of points,
-- we calculate the corners of the rectangle formed from them,
-- then determine if *all* of the corners are within the polygon.
-- If they are, then the rectangle itself is within the polygon,
-- so it is valid.
solve :: [Vertex] -> Integer
solve vertices = maximum . map rectangleSize . filter validRectangle . combinations $ vertices
  where polygon = createPolygon vertices
        validRectangle = all (`insidePolygon` polygon) . rectanglePoints

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day9-test.txt"
  -- print (combinations . parseInput $ contents)
  print (solve . parseInput $ contents)
