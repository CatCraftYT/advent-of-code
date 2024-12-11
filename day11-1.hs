import Data.List.Extra ( splitOn )

toList :: (a,a) -> [a]
toList (x,y) = [x,y]

parseStones :: String -> [Int]
parseStones = map read . splitOn " "

blink :: [Int] -> [Int]
blink = concatMap changeStone
    where changeStone x
            | x == 0 = [1]
            | even len = map read $ (toList.splitAt (len `div` 2)) str
            | otherwise = [x * 2024]
            where str = show x
                  len = length str

main = do
    contents <- readFile "inputs/day11.txt"
    print $ length . last . take 26 . iterate blink . parseStones $ contents
