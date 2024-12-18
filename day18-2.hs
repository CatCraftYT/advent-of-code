import Data.Graph
import Data.Maybe ( isNothing, fromJust )
import Data.Tuple.Extra ( snd3 )

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

endIsReachable :: Maze -> Bool
endIsReachable m = path (graph m) startV endV
    where keyToVertex = fromJust.keyToVertexF m
          startV = keyToVertex $ startPoint m
          endV = keyToVertex $ endPoint m

main = do
    positions <- parsePositions . lines <$> readFile "inputs/day18.txt"
    print $ (positions !!) . subtract 1 . length . takeWhile endIsReachable . scanl (flip deleteNode) (createMaze 70 70) $ positions
