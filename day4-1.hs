import Data.List.Extra ( transpose, (!?), isPrefixOf )
import Data.Maybe ( catMaybes )

getRows :: [String] -> [String]
getRows = id

getCols :: [String] -> [String]
getCols = transpose

getLeftDiagonals :: [String] -> [String]
getLeftDiagonals m = [catMaybes [m !! i !? (i + offset) | i <- rows] | offset <- offsets]
    where rows = [0..length m - 1]
          offsets = [(negate.length) m + 1 .. (length.head) m - 1]

getRightDiagonals :: [String] -> [String]
getRightDiagonals m = [(reverse.catMaybes) [m !! row !? (col - row) | row <- rows] | col <- cols]
    where nRows = length m - 1
          nCols = (length.head) m - 1
          rows = [0..nRows]
          cols = [0..nRows + nCols]

getAllDirections :: [String] -> [String]
getAllDirections m = getRows m ++ getCols m ++ getLeftDiagonals m ++ getRightDiagonals m

countWords :: String -> String -> Int
countWords word s
    | length s < length word = 0
    | word `isPrefixOf` s = 1 + countWords word (tail s)
    | otherwise = countWords word (tail s)

countWordInstances :: String -> [String] -> Int
countWordInstances word words = sum $ countWordInstances' word words
    where countWordInstances' :: String -> [String] -> [Int]
          countWordInstances' _ [] = []
          countWordInstances' word (s:words) = (count + backwardsCount) : countWordInstances' word words
            where count = countWords word s
                  backwardsCount = countWords (reverse word) s

main = do
    contents <- lines <$> readFile "inputs/day4.txt"
    print $ (countWordInstances "XMAS" . getAllDirections) contents
