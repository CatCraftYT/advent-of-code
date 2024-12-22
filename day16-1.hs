import Data.Graph
import Data.Maybe ( catMaybes, isNothing, fromJust, isJust, fromMaybe, mapMaybe )
import Data.List.Extra ( (!?), find, group, minimumBy )
import Data.Tuple.Extra ( fst3, snd3, thd3 )
import Data.Set ( Set )
import Data.Set qualified as S
import Data.IntMap.Strict ( IntMap )
import Data.IntMap.Strict qualified as M
import Data.Function (on)

import Data.Bifunctor (second)

type Position = (Int,Int)
data Maze = Maze {
    startPoint :: Position,
    endPoint :: Position,
    graph :: Graph,
    vertexToNodeF :: Vertex -> (Char, Position, [Position]),
    keyToVertexF :: Position -> Maybe Vertex
}
data Direction = U | D | L | R deriving (Eq,Show)

parseMaze :: [String] -> Maze
parseMaze m = Maze start end graph nodeFromVertex vertexFromKey
    where getNeighbours x y = filter ((/='#').getPartial) $ catMaybes [getNeighbour (x+1,y), getNeighbour (x-1,y), getNeighbour (x,y+1), getNeighbour (x,y-1)]
          getPartial (x,y) = m !! y !! x
          getNeighbour (x,y)
            | isNothing element = Nothing
            | otherwise = Just (x,y)
            where element = (m !? y) >>= (!? x)
          nodes = concat $ [[(c,(x,y),getNeighbours x y) | (x,c) <- zip [0..] row, c /= '#'] | (y,row) <- zip [0..] m]
          (graph, nodeFromVertex, vertexFromKey) = graphFromEdges nodes
          start = snd3.fromJust.find ((=='S').fst3) $ nodes
          end = snd3.fromJust.find ((=='E').fst3) $ nodes

--getDirections :: (Ord a, Ord b) => [(a, b)] -> [Direction]
getDirections :: [(Int, Int)] -> [Direction]
getDirections [] = []
getDirections [x] = []
getDirections ((x1,y1):(x2,y2):ps)
    | x1 == x2 && y1 < y2 = D : getDirections ((x2,y2):ps)
    | x1 == x2 && y1 > y2 = U : getDirections ((x2,y2):ps)
    | x1 < x2 && y1 == y2 = R : getDirections ((x2,y2):ps)
    | x1 > x2 && y1 == y2 = L : getDirections ((x2,y2):ps)

isHomogenous :: (Eq a) => [a] -> Bool
isHomogenous l = all (==head l) l

infinity :: Float
infinity = read "Infinity"

--djikstra :: Maze -> Int
--djikstra m = round $ djikstra' m (M.singleton start start) (S.fromList.vertices.graph $ m) (M.singleton start 0)
--    where target = fromJust.keyToVertexF m.endPoint $ m
--          start = fromJust.keyToVertexF m.startPoint $ m
--          djikstra' :: Maze -> IntMap Vertex -> Set Vertex -> IntMap Float -> Float
--          djikstra' m prevs unvisited dists
--            | current == target = currentDist
--            | otherwise = djikstra' m newPrevs (S.delete current unvisited) newDists
--            where current = minimumBy (compare `on` (fromMaybe infinity . flip M.lookup dists)) unvisited
--                  currentDist = fromJust $ M.lookup current dists
--                  neighbours v = filter (`S.member` unvisited) . map (fromJust.keyToVertexF m) . thd3 $ vertexToNodeF m v
--                  neighboursDists = map ((currentDist+).weight current) $ neighbours current
--                  betterNeighbours = filter (\(v,d) -> d <= fromMaybe d (M.lookup v dists)) $ zip (neighbours current) neighboursDists
--                  newDists = foldr (\(v,d) newDists -> M.alter (Just . const d) v newDists) dists betterNeighbours
--                  newPrevs = foldr (\(v,_) newPrevs -> M.alter (Just . const current) v newPrevs) prevs betterNeighbours
--                  prev = fromJust.M.lookup current $ prevs
--                  weight v1 v2
--                    | prev == v1 && isEastStart = 0
--                    | prev == v1 = 1000
--                    | isTurn = 1001
--                    | otherwise = 1
--                    where isTurn = not.isHomogenous.getDirections.map (snd3.vertexToNodeF m) $ [prev, v1, v2]
--                          isEastStart = [R] == (getDirections.map (snd3.vertexToNodeF m) $ [v1, v2])

-- 148628 - too high
-- 147627 - too low
-- test should be 4013 - https://www.reddit.com/r/adventofcode/comments/1hfhgl1/comment/m2d6ohv
main = do
    contents <- lines <$> readFile "inputs/day16-test.txt"
    traverse print $ djikstra . parseMaze $ contents

djikstra m = reverse . map (second (snd3.vertexToNodeF m)) . catMaybes . takeWhile ((/=start).snd.fromJust) $ iterate ((`M.lookup` result) . snd.fromJust) (Just (0,target))
    where target = fromJust.keyToVertexF m.endPoint $ m
          start = fromJust.keyToVertexF m.startPoint $ m
          result = djikstra' m (M.singleton start (0,start)) (S.fromList.vertices.graph $ m) (M.singleton start 0)
          djikstra' :: Maze -> IntMap (Float,Vertex) -> Set Vertex -> IntMap Float -> IntMap (Float, Vertex)
          djikstra' m prevs unvisited dists
            | current == target = prevs
            | otherwise = djikstra' m newPrevs (S.delete current unvisited) newDists
            where current = minimumBy (compare `on` (fromMaybe infinity . flip M.lookup dists)) unvisited
                  currentDist = fromJust $ M.lookup current dists
                  neighbours = filter (`S.member` unvisited) . map (fromJust.keyToVertexF m) . thd3 $ vertexToNodeF m current
                  neighboursDists = map ((currentDist+).weight current) neighbours
                  betterNeighbours = filter (\(v,d) -> d < fromMaybe infinity (M.lookup v dists)) $ zip neighbours neighboursDists
                  newDists = foldr (\(v,d) newDists -> M.alter (Just . const d) v newDists) dists betterNeighbours
                  newPrevs = foldr (\(v,d) newPrevs -> M.alter (Just . const (d,current)) v newPrevs) prevs betterNeighbours
                  prev = fromJust.M.lookup current $ prevs
                  weight v1 v2
                    | snd prev == v1 && isEastStart = 0
                    | snd prev == v1 = 1001
                    | isTurn = 1001
                    | otherwise = 1
                    where isTurn = not.isHomogenous.getDirections.map (snd3.vertexToNodeF m) $ [snd prev, v1, v2]
                          isEastStart = [R] == (getDirections.map (snd3.vertexToNodeF m) $ [v1, v2])
