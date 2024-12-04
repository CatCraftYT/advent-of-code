import Data.List (inits, tails, isSuffixOf, isPrefixOf, isInfixOf)
import Data.List (intercalate)
import Text.Regex.TDFA

getValidSegments :: String -> [String]
--getValidSegments str = filter (not.containsWithin "don't()") segmentsWithDupes
getValidSegments str = dontSegments
    where endings = filter ("don't()" `isSuffixOf`) $ inits str
          dontSegments = head endings : tail [head $ filter ("don't()" `isPrefixOf`) $ tails substr | substr <- endings]
          segmentsWithDupes = [head $ filter ("do()" `isPrefixOf`) $ tails substr | substr <- dontSegments]

-- Will match strings that start with the search string but it doesn't matter
-- because in this case the strings always start with 'do()'
containsWithin :: String -> String -> Bool
containsWithin search str
    | length str < length search = False
    | search `isPrefixOf` str && str /= search = True
    | otherwise = containsWithin search $ tail str

addValidityMarkers :: String -> String
addValidityMarkers s = "do()" ++ s ++ "don't()"

preprocessString :: String -> String
preprocessString = concat.getValidSegments.addValidityMarkers

-- Solution here is identical to part 1
getMulPairs :: String -> [[String]]
getMulPairs s = map tail $ s =~ "mul\\(([0-9]+),([0-9]+)\\)"

sumMulPairs :: [[String]] -> Int
sumMulPairs xs = sum [read (head x) * read (last x) | x <- xs]
----

main = do
    contents <- readFile "inputs/day3-test.txt"
    print $ sumMulPairs.getMulPairs.preprocessString $ contents
    putStrLn $ intercalate "\n\n" . getValidSegments.addValidityMarkers $ contents
