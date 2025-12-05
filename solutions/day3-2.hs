module Main where

-- All combinations of `l` with 1 element removed.
removals :: [a] -> [[a]]
removals l = [removal n | n <- [0.. length l - 1]]
  where removal n = pre ++ tail post
          where (pre, post) = splitAt n l

-- Just remove one digit, pick the one that results in the biggest number,
-- and keep doing that until we get 12 digits.
-- I am genuinely so surprised that this works.
searchBank :: String -> String
searchBank = findMaximum
  where finalSize = 12
        findMaximum :: String -> String
        findMaximum l
          | length l <= finalSize = l
          | otherwise = findMaximum (show . (maximum . map read . removals :: String -> Integer) $ l)
          

solve :: [String] -> Int
solve = sum . map (read . searchBank)

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day3.txt"
  print (solve contents)

