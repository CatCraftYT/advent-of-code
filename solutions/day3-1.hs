module Main where

combinations :: String -> [String]
combinations [] = []
combinations (x:xs) = map (\y -> [x,y]) xs ++ combinations xs

solve :: [String] -> Int
solve = sum . map (maximum . map read . combinations)

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day3.txt"
  print (solve contents)
