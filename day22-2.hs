import Data.Bits ( xor )
import Data.Map.Strict ( Map )
import Data.Map.Strict qualified as M
import Data.Maybe ( isJust, listToMaybe, fromMaybe )
import Data.List ( isPrefixOf, stripPrefix, zipWith, tails, maximumBy )
import Data.Function ( on )

type SeqCache = Map [Integer] Integer

evolve :: Integer -> Integer
evolve = s3 . s2 . s1
    where mix v n = v `xor` n
          prune n = n `mod` 16777216
          s1 n = prune $ (n * 64) `mix` n
          s2 n = prune $ (n `div` 32) `mix` n
          s3 n = prune $ (n * 2048) `mix` n

prices :: [Integer] -> [[Integer]]
prices = map (map (`mod` 10) . take 2001 . iterate evolve)

diffList :: Num a => [a] -> [a]
diffList [] = []
diffList [x] = []
diffList (a:b:bs) = b - a : diffList (b:bs)

addSequenceValue :: [[Integer]] -> [[Integer]] -> [Integer] -> SeqCache -> SeqCache
addSequenceValue prices diffs s c
    | isJust $ M.lookup s c = c
    | otherwise = M.insert s (sum.zipWith getSequenceValue prices $ diffs) c
    where getSequenceValue ps ds
            | null ps = 0
            | s `isPrefixOf` ds = fromMaybe 0 . listToMaybe . drop (length s - 1) $ ps
            | otherwise = getSequenceValue (tail ps) (tail ds)

getAllSequences :: [[Integer]] -> [[Integer]]
getAllSequences = concatMap (filter ((==4).length) . map (take 4) . tails)

solve :: [[Integer]] -> Integer
solve ps = snd.maximumBy (compare `on` snd) . M.assocs $ seqValues
    where diffs = map diffList ps
          newPrices = map (drop 1) ps
          seqs = getAllSequences diffs
          seqValues = foldr (addSequenceValue newPrices diffs) M.empty seqs

main = do
    contents <- map read . lines <$> readFile "inputs/day22.txt"
    print $ solve.prices $ contents
