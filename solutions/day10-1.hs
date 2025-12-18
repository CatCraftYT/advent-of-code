module Main where
import Data.List
import Data.Bifunctor
import Data.Maybe

type Button = [Int]
data Machine = Machine { lights :: [Int], buttons :: [Button] } deriving (Show, Eq)

combinations :: [a] -> Int -> [[a]]
combinations l n = filter ((==n) . length) . subsequences $ l

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s
  | null post = [pre]
  | otherwise = pre : splitOn c (tail post)
  where (pre, post) = break (==c) s

add :: Num a => [a] -> [a] -> [a]
add = zipWith (+)

parseInput :: [String] -> [Machine]
parseInput = map parseLine
  where parseLine :: String -> Machine
        parseLine line = Machine lightList buttonList
          where (lightList, postLights) = first (map lightToNumber . filter (`elem` "#.")) . break (==' ') $ line
                (buttonIndexList, _) = first (map (map read . splitOn ',') . splitOn ' ' . init . filter (`notElem` "()")) . break (=='{') . tail $ postLights
                nLights = length lightList
                buttonList = map buttonToList buttonIndexList
                lightToNumber c
                  | c == '#' = 1
                  | c == '.' = 0
                  | otherwise = error "Unexpected char in lights"
                buttonToList b = map (\i -> if i `elem` b then 1 else 0) [0..nLights-1]

solve :: [String] -> Int
solve = sum . map solveMachine . parseInput
  where isValid :: Machine -> [[Int]] -> Bool
        isValid m = (== lights m) . map (`mod` 2) . foldr1 add
        -- Find all subsets of 1 button press, then 2, then 3, etc...
        -- the first one that has a valid result is the minimum
        -- number of button presses.
        solveMachine :: Machine -> Int
        solveMachine m = fst . fromJust . find (any (isValid m) . snd) $ zip [1..] (map (combinations (buttons m)) [1..])

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day10.txt"
  print (solve contents)
