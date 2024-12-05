import Data.List.Extra ( transpose, (!?), isInfixOf )
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

filterWordInstances :: String -> [String] -> [String]
filterWordInstances word words = matches ++ backwardsMatches
    where matches = filter (word `isInfixOf`) words
          backwardsMatches = filter (reverse word `isInfixOf`) words

main = do
    contents <- lines <$> readFile "inputs/day4.txt"
    print $ (length . filterWordInstances "XMAS" . getAllDirections) contents
    print $ (filterWordInstances "XMAS" . getAllDirections) contents
