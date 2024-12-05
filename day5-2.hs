import Data.List.Extra (splitOn, sortBy)

data Rule = Rule Int Int deriving (Show)
type Page = [Int]

rfst :: Rule -> Int
rfst (Rule a _) = a

rsnd :: Rule -> Int
rsnd (Rule _ b) = b

compareUsingRules :: [Rule] -> Int -> Int -> Ordering
compareUsingRules [] _ _ = EQ
compareUsingRules (r:rs) a b
    | rfst r == a && rsnd r == b = LT
    | rsnd r == a && rfst r == b = GT
    | otherwise = compareUsingRules rs a b

parseRule :: String -> Rule
parseRule s = Rule (head broken) (last broken)
    where broken = read <$> splitOn "|" s :: [Int]

parseContents :: [[String]] -> ([Rule], [Page])
parseContents contents = (map parseRule ruleList, map (map read . splitOn ",") pagesList)
    where ruleList = head contents
          pagesList = last contents

sortPages :: [Rule] -> [Page] -> [Page]
sortPages rs = map (sortBy (compareUsingRules rs))

filterBadPages :: [Rule] -> [Page] -> [Page]
filterBadPages rs ps = map snd . filter (uncurry (/=)) . zip ps $ sortPages rs ps

median :: [a] -> a
median l = l !! (length l `div` 2)

main = do
    (rules, pages) <- parseContents . map lines.splitOn "\n\n" <$> readFile "inputs/day5.txt"
    print $ sum.map median $ filterBadPages rules pages
