-- Loosely based off of https://www.reddit.com/r/adventofcode/comments/1hbmu6q/comment/m1hsgae/
import Data.List.Extra ( splitOn )
import Data.Map.Strict ( Map )
import Data.Map.Strict qualified as Map

toList :: (a,a) -> [a]
toList (x,y) = [x,y]

parseStones :: String -> Map Integer Integer
parseStones = Map.fromList . map (\x -> (read x,1)) . splitOn " "

blink :: Map Integer Integer -> Map Integer Integer
blink m = foldl (Map.unionWith (+)) Map.empty $ concatMap (\(x,n) -> map (insertFunc n) $ changeStone x) $ Map.assocs m
    where insertFunc v k = Map.insertWith (+) k v Map.empty

changeStone :: Integer -> [Integer]
changeStone x
    | x == 0 = [1]
    | even len = map read $ (toList.splitAt (len `div` 2)) str
    | otherwise = [x * 2024]
    where str = show x
          len = length str

getCount :: Map Integer Integer -> Integer
getCount = Map.foldl (+) 0

main = do
    contents <- readFile "inputs/day11.txt"
    --traverse (print . getCount) $ take 76 . iterate blink . parseStones $ contents
    print $ getCount . last . take 76 . iterate blink . parseStones $ contents
