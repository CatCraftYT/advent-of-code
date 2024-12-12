import Data.Graph
import Data.Tree ( flatten )
import Data.List ( nubBy )
import Data.Maybe ( fromJust )

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

get :: [[a]] -> (Int, Int) -> a
get m (i,j) = m !! i !! j

isSameRegion :: Eq a => [a] -> [a] -> Bool
isSameRegion l1 l2 = all (`elem` l2) l1

getConnectedNeighbours :: [String] -> Int -> Int -> Int -> Int -> [(Int, Int)]
getConnectedNeighbours m x y maxX maxY = filter predicate [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    where predicate p = ((fst p >= 0 && fst p < maxX) && (snd p >= 0 && snd p < maxY)) && (get m p == get m (x,y))

makeGraph :: [String] -> (Graph, Vertex -> (Char, Int, [Int]), Int -> Maybe Vertex)
makeGraph m = graphFromEdges.concat $ [[(c, posToKey (i,j), neighboursToKeys i j) | (j,c) <- zip [0..] row] | (i,row) <- zip [0..] m]
    where posToKey (i, j) = nCols * i + j
          nCols = length.head $ m
          nRows = length m
          neighboursToKeys i j = map posToKey $ getConnectedNeighbours m i j nRows nCols

getRegions :: Graph -> [[Int]]
getRegions = nubBy isSameRegion . map flatten . dff

getPerimeter :: (Vertex -> (Char, Int, [Int])) -> [Int] -> Int
getPerimeter nodeFromVertex = sum.map ((4-).length.thd3.nodeFromVertex)

getArea :: [Int] -> Int
getArea = length

solve :: [String] -> Int
solve m = sum.map (\r -> getArea r * getPerimeter nodeFromVertex r) . getRegions $ graph
    where (graph, nodeFromVertex, _) = makeGraph m

main = do
    contents <- lines <$> readFile "inputs/day12.txt"
    print $ solve contents
