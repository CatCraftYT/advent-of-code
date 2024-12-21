import Data.Graph
import Data.Maybe ( catMaybes, fromJust, isNothing, fromMaybe, mapMaybe, isJust )
import Data.List.Extra ( find, (!?), minimumBy, sort, inits )
import Data.Tuple.Extra ( fst3, snd3, thd3 )
import Data.Set ( Set )
import Data.Set qualified as S
import Data.IntMap.Strict ( IntMap )
import Data.IntMap.Strict qualified as M
import Data.Function ( on )

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
          nodes = concat $ [[(c,(x,y),getNeighbours x y) | (x,c) <- zip [0..] row, c /= '#'] | (y,row) <- zip [0..] m]
          (graph, nodeFromVertex, vertexFromKey) = graphFromEdges nodes
          start = snd3.fromJust.find ((=='S').fst3) $ nodes
          end = snd3.fromJust.find ((=='E').fst3) $ nodes

-- Since there are no forks we can just traverse neighbours
getPath :: Maze -> Path
getPath m = getPath' start start
    where start = fromJust.keyToVertexF m $ startPoint m
          end = fromJust.keyToVertexF m $ endPoint m
          getPath' pv v
            | v == end = []
            | otherwise = snd3 node : getPath' v (fromJust.keyToVertexF m.head.filter (/=prevPos).thd3 $ node)
            where node = vertexToNodeF m v
                  prevPos = snd3 $ vertexToNodeF m pv

solve :: Maze -> Int
solve m = length . filter (>= 100) . map ((length path -) . (+1) . length) $ allJumps
    where allJumps = concatMap (\p -> map (\j -> takeWhile (/=p) path ++ p : dropWhile (/=j) path) . getJumps $ p) path
          path = getPath m
          getJumps (x,y) = mapMaybe (fmap (snd3.vertexToNodeF m) . keyToVertexF m) [(x+2,y), (x-2,y), (x,y+2), (x,y-2)]

main = do
    maze <- parseMaze . lines <$> readFile "inputs/day20.txt"
    print $ solve maze
