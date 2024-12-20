import Data.Graph
import Data.Maybe ( catMaybes, isNothing, fromJust, isJust, fromMaybe, mapMaybe )
import Data.List.Extra ( (!?), find, group, minimumBy )
import Data.Tuple.Extra ( fst3, snd3, thd3 )
import Data.Set ( Set )
import Data.Set qualified as S
import Data.IntMap.Strict ( IntMap )
import Data.IntMap.Strict qualified as M

type Position = (Int,Int)
type Path = [Position]
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
          nodes = concat $ [[(c,(x,y),getNeighbours x y) | (x,c) <- zip [0..] row] | (y,row) <- zip [0..] m]
          (graph, nodeFromVertex, vertexFromKey) = graphFromEdges nodes
          start = snd3.fromJust.find ((=='S').fst3) $ nodes
          end = snd3.fromJust.find ((=='E').fst3) $ nodes

getAllPaths :: Maze -> [Path]
getAllPaths m = map (reverse.fromJust) . filter isJust . searchFrom [] $ startPoint m
    where searchFrom h p
            | p == endPoint m = [Just (p:h)]
            | null unvisitedNodes = [Nothing]
            | otherwise = concatMap (searchFrom (p:h)) unvisitedNodes
            where (_,_,edges) = vertexToNodeF m . fromJust $ keyToVertexF m p
                  unvisitedNodes = filter (`notElem` h) edges

getDirections :: (Ord a, Ord b) => [(a, b)] -> [Direction]
getDirections [] = []
getDirections [x] = []
getDirections ((x1,y1):(x2,y2):ps)
    | x1 == x2 && y1 < y2 = D : getDirections ((x2,y2):ps)
    | x1 == x2 && y1 > y2 = U : getDirections ((x2,y2):ps)
    | x1 < x2 && y1 == y2 = R : getDirections ((x2,y2):ps)
    | x1 > x2 && y1 == y2 = L : getDirections ((x2,y2):ps)

isHomogenous :: (Eq a) => [a] -> Bool
isHomogenous l = all (==head l) l

bulkAlter :: IntMap v -> [(Int, v)] -> IntMap v
bulkAlter = foldr (\(k,v) m -> M.alter (const $ Just v) k m)

infinity :: Float
infinity = read "Infinity"

-- Implementation of https://en.wikipedia.org/wiki/A*_search_algorithm#Pseudocode
aStar :: Maze -> Path
aStar m = reverse . map (snd3.vertexToNodeF m) . catMaybes . takeWhile isJust $ iterate ((`M.lookup` result) . fromJust) (keyToVertexF m $ endPoint m)
    where heuristic (x,y) = abs (fromIntegral x - (fromIntegral.fst.endPoint) m) + abs (fromIntegral y - (fromIntegral.snd.endPoint) m)
          start = fromJust.keyToVertexF m $ startPoint m
          end = fromJust.keyToVertexF m $ endPoint m
          result = aStar' (S.singleton start) M.empty (M.singleton start 0) (M.singleton start (heuristic.startPoint $ m))
          aStar' :: Set Vertex -> IntMap Int -> IntMap Float -> IntMap Float -> IntMap Int
          aStar' openSet cameFrom gScore fScore
            | current == end = cameFrom
            | otherwise = aStar' newOpenSet newCameFrom newGScore newFScore
            where current = minimumBy (\a b -> M.lookup a fScore `compare` M.lookup b fScore) openSet
                  currentPos = snd3.vertexToNodeF m $ current
                  neighboursPos = thd3.vertexToNodeF m $ current
                  neighbours = mapMaybe (keyToVertexF m) neighboursPos
                  improvedNeighbours = filter (\(v,g) -> g < (fromMaybe infinity . M.lookup v) gScore) $ zip neighbours [(fromJust . M.lookup current) gScore + weight n | n <- neighboursPos]
                  newGScore = bulkAlter gScore improvedNeighbours
                  newFScore = bulkAlter fScore $ map (\(v,g) -> (v, g + (heuristic.snd3.vertexToNodeF m $ v))) improvedNeighbours
                  newCameFrom = bulkAlter cameFrom $ map (\(v,_) -> (v,current)) improvedNeighbours
                  newOpenSet = S.delete current $ foldr (\(v,_) s -> S.insert v s) openSet improvedNeighbours
                  weight n
                    | isNothing preceding && (head.getDirections) [currentPos, n] /= R = 1000
                    | isNothing preceding = 1
                    | isHomogenous.getDirections $ [fromJust preceding, currentPos, n] = 1
                    | otherwise = 1000
                    where preceding = snd3.vertexToNodeF m <$> M.lookup current cameFrom

getPathCost :: Path -> Int
getPathCost p = (length.concat $ straights) + length straights * 1000
    where straights = group $ getDirections p

showPath :: [String] -> Path -> [String]
showPath ss p = [[if (x,y) `elem` p then 'O' else col | (x,col) <- zip [0..] row] | (y,row) <- zip [0..] ss]

-- 148628 - too high
-- test should be 4103 - https://www.reddit.com/r/adventofcode/comments/1hfhgl1/comment/m2d6ohv
main = do
    contents <- lines <$> readFile "inputs/day16-test.txt"
    traverse print $ showPath contents . aStar . parseMaze $ contents
    print $ getPathCost . aStar . parseMaze $ contents
