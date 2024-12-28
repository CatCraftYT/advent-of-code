import Data.Bits
import Data.Bifunctor ( second )
import Data.Map.Lazy ( Map )
import Data.Map.Lazy qualified as M
import Data.List ( isPrefixOf )
import Data.Maybe (fromJust, isNothing)

type WireMap = Map String Int

unsafeLookup :: Ord k => k -> Map k v -> v
unsafeLookup k m = fromJust $ M.lookup k m

parseConnection :: String -> (String, Int -> Int -> Int, String, String)
parseConnection c = (take 3 c, op, take 3 remaining, drop 7 remaining)
    where op = fst opPair
          remaining = snd opPair
          opPair
            | "XOR" `isPrefixOf` str = (xor, drop 4 str)
            | "AND" `isPrefixOf` str = ((.&.), drop 4 str)
            | "OR" `isPrefixOf` str = ((.|.), drop 3 str)
            where str = drop 4 c

parseWireMap :: [String] -> [String] -> WireMap
parseWireMap ins conns = result
    where result = foldr (\(i1, op, i2, out) m -> M.insert out (unsafeLookup i1 result `op` unsafeLookup i2 result) m) (M.fromList inputTuples) connTuples
          parseInput i = (take 3 i, read . drop 5 $ i)
          inputTuples = map parseInput ins
          connTuples = map parseConnection conns

-- Since we're using lazy maps, we can just read each output value and
-- Haskell will automagically evaluate all the dependencies. Isn't that cool?
solve :: WireMap -> Int
solve m = sum [b * (2^n) | (n,b) <- zip [0..] outputs]
    where outputs = M.elems . M.filterWithKey (\k _ -> "z" `isPrefixOf` k) $ m

main = do
    (inputs, connections) <- second (drop 1) . break null . lines <$> readFile "inputs/day24.txt"
    print $ solve . parseWireMap inputs $ connections
