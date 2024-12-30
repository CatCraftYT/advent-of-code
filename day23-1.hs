import Data.Graph
import Data.Bifunctor ( second, bimap )
import Data.List ( break, nub, sortBy, isPrefixOf )
import Data.Maybe ( fromJust )
import Data.Map.Strict qualified as M
import Data.Array ( assocs )
import Data.Function ( on )

data Network = Network {
    graph :: Graph,
    vertexToNameF :: Vertex -> String,
    adjacentVerticesF :: Vertex -> [Vertex]
}

invertMap :: Ord v => M.Map k v -> M.Map v k
invertMap = M.fromList . map swap . M.assocs
    where swap (a,b) = (b,a)

parseConnections :: [String] -> Network
parseConnections s = Network (buildG (0, length nodeSet - 1) edges) vertexToNameF adjacentVerticesF
    where pairs = map (second (drop 1) . break (=='-')) s
          edges = concatMap ((\(a,b) -> [(a,b),(b,a)]) . bimap nameToVertex nameToVertex) pairs
          nameToVertex = fromJust . (`M.lookup` nameToVertexMap)
          nodeSet = nub $ map fst pairs ++ map snd pairs
          nameToVertexMap = M.fromList $ zip nodeSet [0..]
          vertexToNameF = fromJust.(`M.lookup` invertMap nameToVertexMap)
          adjacentVerticesF v = map snd . filter ((==v).fst) $ edges

-- https://en.wikipedia.org/wiki/Clique_problem#Cliques_of_fixed_size
-- Triangle finding algorithm from "Arboricity and subgraph listing algorithms"
getTriangles :: Network -> [[Vertex]]
getTriangles n = getTriangles' sortedDegrees []
    where g = graph n
          adjacentVertices = adjacentVerticesF n
          sortedDegrees = reverse . map fst . sortBy (compare `on` snd) . assocs . outdegree $ g
          getTriangles' [] _ = []
          getTriangles' (v:vs) excluded = getTrianglesContaining v (adjacentVertices v) excluded ++ next
            where next = getTriangles' vs (v : excluded)
                  getTrianglesContaining _ [] _ = []
                  getTrianglesContaining vi (u:marked) excluded
                      | u `elem` excluded = next
                      | otherwise = triangles ++ next
                      where markedNeighbours = filter (`elem` marked) . filter (`notElem` excluded) $ adjacentVertices u
                            triangles = map (\w -> [vi,u,w]) markedNeighbours
                            next = getTrianglesContaining vi marked excluded

solve :: Network -> Int
solve n = length . filter id . map (any (("t" `isPrefixOf`) . vertexToNameF n)) . getTriangles $ n

main = do
    contents <- lines <$> readFile "inputs/day23.txt"
    print $ solve . parseConnections $ contents
