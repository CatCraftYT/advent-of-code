module Main where
import Data.Graph
import Data.Maybe
import Data.Bifunctor
import Control.Monad.State.Lazy
import qualified Data.IntMap.Strict as IntMap

type GraphBundle = (Graph, Vertex -> (Int, String, [String]), String -> Maybe Vertex)
type Cache = IntMap.IntMap Integer

-- From day 6
splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s
  | null post = [pre]
  | otherwise = pre : splitOn c (tail post)
  where (pre, post) = break (==c) s

parseInput :: [String] -> GraphBundle
parseInput = graphFromEdges . ((0,"out",[]):) . map stringToNode
  where stringToNode s = (0,key,nodeEdges)
          where (key, post) = break (==':') s
                nodeEdges = splitOn ' ' $ drop 2 post

-- DFS with memoization
-- Only keep track of the number of paths from start to end
-- through each node, because that's what we need and
-- keeping every path would blow up my memory
nPaths :: GraphBundle -> String -> String -> Integer
nPaths (_, nodeFromVertex, vertexFromKey) end = (`evalState` IntMap.empty) . getPaths [] . fromJust . vertexFromKey
  where endVertex = fromJust (vertexFromKey end)
        keysToVertices = map (fromJust . vertexFromKey)
        getPaths :: [Vertex] -> Vertex -> State Cache Integer
        getPaths prev cur = state getPaths'
          where getPaths' cache
                  | isJust cacheLookup         = (cacheResult, cache)
                  | cur `elem` prev            = (0, IntMap.insertWith (+) cur 0 cache)
                  | endVertex `elem` connected = (1, IntMap.insertWith (+) cur 1 cache)
                  | otherwise                  = (paths, IntMap.insertWith (+) cur paths updatedCache)
                  where (_,_,connectedKeys) = nodeFromVertex cur
                        connected = keysToVertices connectedKeys
                        cacheLookup = IntMap.lookup cur cache
                        cacheResult = fromJust cacheLookup
                        (paths, updatedCache) = foldr (\next (count, s) -> first (+count) $ runState (getPaths (cur : prev) next) s) (0,cache) connected

-- Number of valid paths = (number of paths from svr to dac) *
--                         (number of paths from dac to fft) *
--                         (number of paths from fft to out) +
--                         (number of paths from svr to fft) *
--                         (number of paths from fft to dac) *
--                         (number of paths from dac to out) 
solve :: GraphBundle -> Integer
solve g = (sd * df * fo) + (sf * fd * dto)
  where sd = nPaths g "dac" "svr"
        df = nPaths g "fft" "dac"
        fo = nPaths g "out" "fft"
        sf = nPaths g "fft" "svr"
        fd = nPaths g "dac" "fft"
        dto = nPaths g "out" "dac"

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day11.txt"
  print (solve . parseInput $ contents)
