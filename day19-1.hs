import Data.List.Extra ( stripInfix, splitOn, isPrefixOf, stripPrefix )
import Data.Bifunctor ( bimap )
import Data.Maybe ( fromJust )

--solve :: [String] -> [String] -> Int
solve towels = map solve'
    where solve' design
            | null possible = False
            | all null nextDesigns = True
            | otherwise = any solve' nextDesigns
            where possible = filter (`isPrefixOf` design) towels
                  nextDesigns = map (fromJust . flip stripPrefix design) possible

main = do
    (towels, designs) <- bimap (splitOn ", " . filter (/='\n')) lines . fromJust . stripInfix "\n\n" <$> readFile "inputs/day19.txt"
    traverse print $ solve towels designs
