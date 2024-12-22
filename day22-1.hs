import Data.Bits ( xor )

evolve :: Integer -> Integer
evolve = s3 . s2 . s1
    where mix v n = v `xor` n
          prune n = n `mod` 16777216
          s1 n = prune $ (n * 64) `mix` n
          s2 n = prune $ (n `div` 32) `mix` n
          s3 n = prune $ (n * 2048) `mix` n

main = do
    contents <- map read . lines <$> readFile "inputs/day22.txt"
    print $ sum . map (last . take 2001 . iterate evolve) $ contents
