import Data.List (inits, tails, isSuffixOf, isPrefixOf, isInfixOf)
import Text.Regex.TDFA

getValidSegments :: String -> [String]
getValidSegments str = reverse.removePrecedingSubstrings $ reverse segmentsWithDupes
    where starts = filter ("do()" `isPrefixOf`) $ tails str
          segmentsWithDupes = [head $ filter ("don't()" `isSuffixOf`) $ inits substr | substr <- starts]

removePrecedingSubstrings :: [String] -> [String]
removePrecedingSubstrings [x] = [x]
removePrecedingSubstrings (x:xs)
    | any (x `isInfixOf`) xs = removePrecedingSubstrings xs
    | otherwise = x : removePrecedingSubstrings xs

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
    contents <- readFile "inputs/day3.txt"
    print $ sumMulPairs.getMulPairs.preprocessString $ contents
