module Main where

type Dial = Int
data DialTurn = TurnLeft Int | TurnRight Int

parseDialTurn :: String -> DialTurn
parseDialTurn s
  | null s = error "Empty DialTurn string"
  | head s == 'L' = TurnLeft (read . tail $ s)
  | head s == 'R' = TurnRight (read . tail $ s)
  | otherwise = error ("Invalid DialTurn string: " ++ s)

turnDial :: Dial -> DialTurn -> Dial
turnDial dial (TurnLeft n) = (dial - n) `mod` 100
turnDial dial (TurnRight n) = (dial + n) `mod` 100

solve :: [String] -> Int
solve = length . filter (==0) . scanl turnDial 50 . map parseDialTurn

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day1.txt"
  print (solve contents)
