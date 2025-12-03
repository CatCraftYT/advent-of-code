module Main where

type Dial = Int
data DialTurn = TurnLeft Int | TurnRight Int

isLeft :: DialTurn -> Bool
isLeft (TurnLeft _) = True
isLeft _ = False

isRight :: DialTurn -> Bool
isRight (TurnRight _) = True
isRight _ = False

parseDialTurn :: String -> DialTurn
parseDialTurn s
  | null s = error "Empty DialTurn string"
  | head s == 'L' = TurnLeft (read . tail $ s)
  | head s == 'R' = TurnRight (read . tail $ s)
  | otherwise = error ("Invalid DialTurn string: " ++ s)

solve :: [String] -> Int
solve = fst . foldl foldFunction (0, 50) . map parseDialTurn
  where foldFunction (nZeroes, pos) (TurnLeft n)
          -- For left turns, take into account that
          -- we could end at a zero without wrapping (add it to the total).
          -- Also, we don't count the first left wrap if we started at zero.
          | pos == 0  = (nZeroes + abs d - 1 + endingZeroFactor, m)
          | otherwise = (nZeroes + abs d + endingZeroFactor, m)
          where (d, m) = (pos - n) `divMod` 100
                endingZeroFactor
                  | m == 0 = 1
                  | otherwise = 0
        -- We can never reach zero with a right turn unless we wrap,
        -- so the number of zeroes is just the number of wraps.
        foldFunction (nZeroes, pos) (TurnRight n) = (nZeroes + abs d, m)
          where (d, m) = (pos + n) `divMod` 100

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day1.txt"
  print (solve contents)
