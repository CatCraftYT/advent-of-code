import Data.Maybe ( mapMaybe, isNothing, fromJust )
import Data.List ( nubBy )
import Data.Map.Strict ( Map )
import Data.Map.Strict qualified as Map

type Position = (Int, Int)
type TopoMap = Map Position Int

parseTopoMap :: [String] -> TopoMap
parseTopoMap m = Map.fromList $ concat [[((i,j), read [col]) | (j, col) <- zip [0..] row] | (i, row) <- zip [0..] m]

accessibleTo :: Maybe Int -> Maybe Int -> Bool
accessibleTo a b
    | isNothing a || isNothing b = False
    | abs (p1 - p2) /= 1 = False
    | otherwise = p1 > p2
    where p1 = fromJust a
          p2 = fromJust b

getTrails :: TopoMap -> Position -> [[Position]]
getTrails m = filter (\t -> (fromJust . Map.lookup (last t)) m == 9) . getAllTrails m
    where getAllTrails m (x,y)
            | null nextPositions = [[(x,y)]]
            | otherwise = map ((x,y) :) $ concatMap (getTrails m) nextPositions
            where adjacentPositions = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
                  nextPositions = filter (\p -> Map.lookup p m `accessibleTo` Map.lookup (x,y) m) adjacentPositions

solve :: TopoMap -> Int
solve m = sum . map (length.getTrails m) . Map.keys $ Map.filter (==0) m

main = do
    contents <- lines <$> readFile "inputs/day10.txt"
    print $ solve.parseTopoMap $ contents
