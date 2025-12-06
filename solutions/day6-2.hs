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

-- Use the distances between operators to determine where the next column is
getColumns :: String -> String -> [String]
getColumns ops cols
  | null next = [cur]
  | otherwise = init cur : getColumns post next
  where (pre, post) = break (/=' ') (tail ops)
        distance = length pre + 1
        (cur, next) = splitAt distance cols

solve :: [String] -> Integer
-- solve rows = sum . map (\(ns, op) -> foldl1 op ns) $ zip numbers ops
solve rows = sum (zipWith foldl1 ops numbers)
  where splitRows = map (filter (not . null) . splitOn ' ') rows
        -- Get columns, e.g. [["123", " 45", "  6"], ...]
        columns = transpose . map (getColumns (last rows)) $ init rows
        -- Extract the vertical numbers from those columns,
        -- e.g. [["1  ", "24 ", "356"], ...]
        -- Then remove the spaces and convert to integers
        numbers = map (map (read . filter (/=' ')) . transpose) columns
        numbers :: [[Integer]]
        ops = map (charToOp . head) (last splitRows)

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day6.txt"
  print (solve contents)
