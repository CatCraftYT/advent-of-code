import Data.List.Extra (tails, elem, splitOn, intercalate, transpose)
import Data.Maybe (catMaybes)

data MovementDirection = U | D | L | R deriving (Show, Eq)

getMovementDirection :: [String] -> MovementDirection
getMovementDirection strs = head.head.filter (not.null) $ [catMaybes [arrowDirection col | col <- row] | row <- strs]
    where arrowDirection c
            | c == head "^" = Just U
            | c == head "v" = Just D
            | c == head "<" = Just L
            | c == head ">" = Just R
            | otherwise = Nothing

getArrow :: MovementDirection -> String
getArrow d
    | d == U = "^"
    | d == D = "v"
    | d == L = "<"
    | d == R = ">"

splitOnFirst :: Eq a => [a] -> [a] -> ([a],[a])
splitOnFirst sp l = (head split, intercalate sp $ tail split)
    where split = splitOn sp l

isArrow :: Char -> Bool
isArrow = flip elem "^>v<"

containsArrow :: String -> Bool
containsArrow = any isArrow

rotateArrow :: String -> String
rotateArrow str = [rotate [c] | c <- str]
    where rotate c
            | c == "^" = head ">"
            | c == ">" = head "v"
            | c == "v" = head "<"
            | c == "<" = head "^"
            | otherwise = head c

-- Pretty nasty but it works
step :: [String] -> [String]
step m
    | dir == U || dir == D = transpose $ step' (transpose m)
    | dir == L || dir == R = step' m
    where dir = getMovementDirection m
          step' m1 = [if containsArrow r then processArrow r else r | r <- m1]
            where processArrow r
                    | dir == U || dir == L = moveLeft
                    | dir == D || dir == R = moveRight
                    where arrow = getArrow dir
                          sp = splitOnFirst arrow r
                          moveLeft
                            | null (fst sp) = "X" ++ snd sp
                            | last (fst sp) == head "#" = fst sp ++ rotateArrow arrow ++ snd sp
                            | otherwise = (init . fst) sp ++ arrow ++ "X" ++ snd sp
                          moveRight
                            | null (snd sp) = fst sp ++ "X"
                            | head (snd sp) == head "#" = fst sp ++ rotateArrow arrow ++ snd sp
                            | otherwise = fst sp ++ "X" ++ arrow ++ (tail . snd) sp

getFinalBoard :: [String] -> [String]
getFinalBoard m = step.last $ takeWhile (any containsArrow) $ iterate step m

countTraversedSpots :: [String] -> Int
countTraversedSpots = sum . map (foldl (\acc c -> if c == head "X" then acc + 1 else acc) 0)

main = do
    contents <- lines <$> readFile "inputs/day6.txt"
    print $ (countTraversedSpots.getFinalBoard) contents
