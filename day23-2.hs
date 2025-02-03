import Data.Graph
import Data.Bifunctor ( second, bimap )
import Data.List ( break, nub, sortBy, isPrefixOf, maximumBy )
import Data.List.NonEmpty qualified as L
import Data.Maybe ( fromJust )
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Array ( assocs )
import Data.Function ( on )
import Data.Tuple.Extra (fst3)

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

-- https://en.wikipedia.org/wiki/Clique_problem#Finding_maximum_cliques_in_arbitrary_graphs
-- https://en.wikipedia.org/wiki/Bron–Kerbosch_algorithm
getMaximumClique :: Network -> [Vertex]
getMaximumClique n = S.toList . maximumBy (compare `on` S.size) $ bronKerbosch S.empty (S.fromList . vertices . graph $ n) S.empty
    where adjacentVertices = S.fromList . adjacentVerticesF n
          bronKerbosch :: S.Set Vertex -> S.Set Vertex -> S.Set Vertex -> [S.Set Vertex]
          bronKerbosch r p x
            | S.null p && S.null x = [r]
            | otherwise = concat . L.toList . L.map fst3 $ L.scanr foldFunc ([], p, x) p
            where pux = p `S.union` x
                  foldFunc v (cs,p,x) = (cs ++ bronKerbosch (v `S.insert` r) (p `S.intersection` adj) (x `S.intersection` adj), v `S.delete` p, v `S.insert` x)
                    where adj = adjacentVertices v

--solve :: Network -> Int
--solve n = length . filter id . map (any (("t" `isPrefixOf`) . vertexToNameF n)) . getMaximumClique $ n

main = do
    contents <- lines <$> readFile "inputs/day23.txt"
    print $ getMaximumClique . parseConnections $ contents
    --print "Scrungle"
