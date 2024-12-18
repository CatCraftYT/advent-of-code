import Data.Graph
import Data.Maybe ( catMaybes, isNothing, fromJust, isJust, fromMaybe, mapMaybe )
import Data.List.Extra ( (!?), minimumBy)
import Data.Tuple.Extra ( fst3, snd3, thd3 )
import Data.Set ( Set )
import Data.Set qualified as S
import Data.IntMap.Strict ( IntMap )
import Data.IntMap.Strict qualified as M

type Position = (Int, Int)
type Path = [Position]
data Maze = Maze {
    startPoint :: Position,
    endPoint :: Position,
    graph :: Graph,
    vertexToNodeF :: Vertex -> (Char, Position, [Position]),
    keyToVertexF :: Position -> Maybe Vertex
}

parsePositions :: [String] -> [Position]
parsePositions = map (\s -> read $ '(' : s ++ ")")

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
                  improvedNeighbours = filter (\(v,g) -> g < (fromMaybe infinity . M.lookup v) gScore) $ zip neighbours [(fromJust . M.lookup current) gScore + 1 | n <- neighboursPos]
                  newGScore = bulkAlter gScore improvedNeighbours
                  newFScore = bulkAlter fScore $ map (\(v,g) -> (v, g + (heuristic.snd3.vertexToNodeF m $ v))) improvedNeighbours
                  newCameFrom = bulkAlter cameFrom $ map (\(v,_) -> (v,current)) improvedNeighbours
                  newOpenSet = S.delete current $ foldr (\(v,_) s -> S.insert v s) openSet improvedNeighbours

deleteNode :: Position -> Maze -> Maze
deleteNode p m
    | isNothing . keyToVertexF m $ p = m
    | otherwise = Maze (startPoint m) (endPoint m) newGraph nodeFromVertex vertexFromKey
    where nodes = filter ((/=p).snd3) . map (vertexToNodeF m) . vertices $ graph m
          (newGraph, nodeFromVertex, vertexFromKey) = graphFromEdges nodes

createMaze :: Int -> Int -> Maze
createMaze xMax yMax = Maze (0,0) (xMax, yMax) graph nodeFromVertex vertexFromKey
    where neighbours x y = filter (\(x,y) -> x <= xMax && y <= yMax) [(x+1, y), (x-1, y), (x, y+1), (x,y-1)]
          (graph, nodeFromVertex, vertexFromKey) = graphFromEdges [('.', (x,y), neighbours x y) | x <- [0..xMax], y <- [0..yMax]]

showPath :: Int -> Int -> Path -> [String]
showPath xMax yMax p = [[if (x,y) `elem` p then 'O' else '.' | x <- [0..xMax]] | y <- [0..yMax]]

showMaze :: Maze -> [String]
showMaze m = [[if keyExists (x,y) then '.' else '#' | x <- [0..fst.endPoint $ m]] | y <- [0..snd.endPoint $ m]]
    where keyExists = isJust.keyToVertexF m

main = do
    contents <- lines <$> readFile "inputs/day18.txt"
    print $ (\x -> x - 1) . length.aStar.foldr deleteNode (createMaze 70 70) . take 1024 . parsePositions $ contents
