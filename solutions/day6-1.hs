module Main where
import Data.List

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s
  | null post = [pre]
  | otherwise = pre : splitOn c (tail post)
  where (pre, post) = break (==c) s

charToOp :: Num a => Char -> (a -> a -> a)
charToOp c
  | c == '+' = (+)
  | c == '*' = (*)
  | otherwise = error ("Parse error on charToOp: " ++ [c])

solve :: [String] -> Integer
-- solve rows = sum . map (\(ns, op) -> foldl1 op ns) $ zip numbers ops
solve rows = sum (zipWith foldl1 ops numbers)
  where splitRows = map (filter (not . null) . splitOn ' ') rows
        numbers = transpose . map (map read) $ init splitRows
        numbers :: [[Integer]]
        ops = map (charToOp . head) (last splitRows)

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day6.txt"
  print (solve contents)
