import Data.List.Extra ( transpose, (!?), isPrefixOf, isInfixOf, tails)
import Data.Maybe ( catMaybes, isNothing, fromJust )

padStrings :: [String] -> [String]
padStrings strs = [(concat.replicate n) " " ++ s | (n,s) <- zip [0..] strs]

padStringsReverse :: [String] -> [String]
padStringsReverse = reverse . (padStrings.reverse)

-- Why fmap doesn't work (properly) on tuples will forever be a mystery...
map2Tuple :: (a -> b) -> (a,a) -> (b,b)
map2Tuple f (a1, a2) = (f a1, f a2)

equalsIncludingReverse :: (Eq a) => [a] -> [a] -> Bool
equalsIncludingReverse a b
    | a == b = True
    | reverse a == b = True
    | otherwise = False

countCrosses :: [String] -> Int
countCrosses strs = length . filter predicate $ crossPairs
    where windows = [take (length "MAS") s | s <- tails strs]
          crossPairs = concatMap (\w -> zip (transpose $ padStrings w) (transpose $ padStringsReverse w)) windows
          predicate (a,b) = a `equalsIncludingReverse` "MAS" && b `equalsIncludingReverse` "MAS"

main = do
    contents <- lines <$> readFile "inputs/day4.txt"
    print $ countCrosses contents
