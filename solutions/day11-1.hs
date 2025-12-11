module Main where
import Data.Graph
import Data.Maybe

type GraphBundle = (Graph, Vertex -> (Int, String, [String]), String -> Maybe Vertex)

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

-- DFS
allPaths :: GraphBundle -> String -> String -> [[String]]
allPaths (_, nodeFromVertex, vertexFromKey) end = getPaths []
  where getPaths :: [String] -> String -> [[String]]
        getPaths prev cur
          | cur `elem` prev = []
          | end `elem` connected = [end : (cur : prev)]
          | otherwise = concatMap (getPaths (cur : prev)) connected
          where (_,_,connected) = nodeFromVertex . fromJust . vertexFromKey $ cur

solve :: GraphBundle -> Int
solve bundle = length $ allPaths bundle "out" "you"

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day11.txt"
  print (solve . parseInput $ contents)
