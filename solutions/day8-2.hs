-- This takes a few minutes to run on the real input.
module Main where
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

type Point = (Int, Int, Int)
type PointPair = (Point,Point)

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

-- For each point, keep track of every connected point.
-- When we add a new edge, update the set of connected points for both of
-- the endpoints by merging their sets and the sets of connected points.
-- When we add an edge that results in the merged set being equal to the set
-- of all points, then we have the desired edge.
-- A disjoint-set data structure might be a good fit for this, but
-- I didn't want to use some random library, and am too lazy to implement
-- it myself. So instead use a map as a ghetto DS data structure.
solve :: [Point] -> Int
solve allPoints = mulXCoords $ getConnectingEdge Map.empty closest
-- solve allPoints = closest
  where closest = sortOn (uncurry distance) . concat $ [[(a, b) | a <- allPoints, a /= b] | b <- allPoints]
        pointSet = Set.fromList allPoints
        mulXCoords ((x1,_,_), (x2,_,_)) = x1*x2
        getConnectingEdge :: Map.Map Point (Set.Set Point) -> [PointPair] -> PointPair
        getConnectingEdge _ [] = error "Empty point pair list"
        getConnectingEdge sets (pair:pairs)
          | abSet == pointSet = pair
          | otherwise = getConnectingEdge updatedSets pairs
          where (a,b) = pair
                aSet = Map.findWithDefault Set.empty a sets
                bSet = Map.findWithDefault Set.empty b sets
                abSet = Set.union aSet bSet
                unionSet = Set.insert a . Set.insert b $ foldr (Set.union . flip (Map.findWithDefault Set.empty) sets) abSet abSet
                updatedSets = Map.insert b unionSet . Map.insert a unionSet $ sets

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day8.txt"
  print (solve . parseInput $ contents)
