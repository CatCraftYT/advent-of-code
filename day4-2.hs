import Data.List.Extra ( transpose, (!?), isPrefixOf, isInfixOf, tails)
import Data.Maybe ( catMaybes, isNothing, fromJust )

getLeftDiagonals :: [String] -> [String]
getLeftDiagonals m = [catMaybes [m !! i !? (i + offset) | i <- rows] | offset <- offsets]
    where rows = [0..length m - 1]
          offsets = [(negate.length) m + 1 .. (length.head) m - 1]

getRightDiagonals :: [String] -> [String]
getRightDiagonals m = [catMaybes [m !! row !? (col - row) | row <- rows] | col <- cols]
    where nRows = length m - 1
          nCols = (length.head) m - 1
          rows = [0..nRows]
          cols = [0..nRows + nCols]

getDiagDirections :: [String] -> [String]
getDiagDirections m = getLeftDiagonals m ++ getRightDiagonals m

countWords :: String -> String -> Int
countWords word s
    | length s < length word = 0
    | word `isPrefixOf` s = 1 + countWords word (tail s)
    | otherwise = countWords word (tail s)

getAllCombinations :: [a] -> [a] -> [(a,a)]
getAllCombinations a b = (,) <$> a <*> b

-- Includes reverse substrings.
findSubstring :: String -> String -> Maybe Int
findSubstring search str = findSubstring' search str 0
    where findSubstring' search str depth
            | length str < length search = Nothing
            | search `isPrefixOf` str || reverse search `isPrefixOf` str = Just depth
            | otherwise = findSubstring' search (tail str) (depth + 1)

countCrosses :: [String] -> [String] -> [(String,String)]
countCrosses dl dr = filter predicate $ getAllCombinations dl dr
    where predicate (s1,s2) = not (isNothing match1 && isNothing match2) && match1 == match2
            where match1 = findSubstring "MAS" s1
                  match2 = findSubstring "MAS" s2

main = do
    contents <- lines <$> readFile "inputs/day4-test.txt"
    print $ getLeftDiagonals contents
    print $ getRightDiagonals contents
    print $ countCrosses (getLeftDiagonals contents) (getRightDiagonals contents)
    print $ length $ countCrosses (getLeftDiagonals contents) (getRightDiagonals contents)
