import Data.List.Extra (tails, elem, intercalate, transpose, find)
import Data.Tuple.Extra (fst3, snd3, thd3)
import Data.Maybe (catMaybes, fromJust)

data MovementDirection = U | R | D | L deriving (Show, Eq)
data TileState = Travelled | Obstacle | Empty | Guard MovementDirection deriving (Show, Eq)
type Tile = (TileState, [MovementDirection])

isArrowString :: Char -> Bool
isArrowString = flip elem "^>v<"

arrowToDirection :: Char -> Maybe MovementDirection
arrowToDirection c
    | c == head "^" = Just U
    | c == head "v" = Just D
    | c == head "<" = Just L
    | c == head ">" = Just R
    | otherwise = Nothing

stringsToTiles :: [String] -> [[Tile]]
stringsToTiles strs = [[charToTile c | c <- row] | row <- strs]
    where charToTile c
            | c == head "." = (Empty, [])
            | c == head "#" = (Obstacle, [])
            | isArrowString c = (Guard (fromJust $ arrowToDirection c), [])

tilesToStrings :: [[Tile]] -> [String]
tilesToStrings strs = [[tileToChar t | t <- row] | row <- strs]
    where tileToChar t
            | isGuard t = directionToArrow $ getGuardDirection t
            | isTravelled t = head "X"
            | isObstacle t = head "#"
            | otherwise = head "."
            where directionToArrow d
                    | d == U = head "^"
                    | d == D = head "v"
                    | d == L = head "<"
                    | d == R = head ">"

isGuard :: Tile -> Bool
isGuard (Guard _, _) = True
isGuard _ = False

isObstacle :: Tile -> Bool
isObstacle (Obstacle, _) = True
isObstacle _ = False

isTravelled :: Tile -> Bool
isTravelled (Travelled, _) = True
isTravelled _ = False

makeTravelled :: Tile -> Tile
makeTravelled (Guard d, h) = (Travelled, h ++ [d])

makeGuard :: Tile -> MovementDirection -> Tile
makeGuard (_, h) d = (Guard d, h)

getGuard :: [[Tile]] -> Tile
getGuard m = snd.head.head.filter (not.null) $ [filter fst [(isGuard col, col) | col <- row] | row <- m]

getMovementDirection :: [[Tile]] -> MovementDirection
getMovementDirection m = getGuardDirection $ getGuard m

getGuardDirection :: Tile -> MovementDirection
getGuardDirection (Guard d, _) = d

-- Assumes the element is in the list (crash otherwise)
splitOnPredicate :: Eq a => (a -> Bool) -> [a] -> ([a],a,[a])
splitOnPredicate p l = (takeWhile (not.p) l, fromJust $ find p l, tail $ dropWhile (not.p) l)

containsGuard :: [Tile] -> Bool
containsGuard = any isGuard

rotate :: MovementDirection -> MovementDirection
rotate d
    | d == U = R
    | d == R = D
    | d == D = L
    | d == L = U

rotateTile :: Tile -> Tile
rotateTile (Guard d, h) = (Guard (rotate d), h)

-- Pretty nasty but it works
step :: [[Tile]] -> [[Tile]]
step m
    | dir == U || dir == D = transpose $ step' (transpose m)
    | dir == L || dir == R = step' m
    where dir = getMovementDirection m
          step' m1 = [if containsGuard r then processArrow r else r | r <- m1]
            where processArrow r
                    | dir == U || dir == L = moveLeft
                    | dir == D || dir == R = moveRight
                    where sp = splitOnPredicate isGuard r
                          moveLeft
                            | null (fst3 sp) = makeTravelled (snd3 sp) : thd3 sp
                            | isObstacle $ last (fst3 sp) = fst3 sp ++ rotateTile (snd3 sp) : thd3 sp
                            | otherwise = (init . fst3) sp ++ makeGuard (last . fst3 $ sp) (getGuardDirection $ snd3 sp) : makeTravelled (snd3 sp) : thd3 sp
                          moveRight
                            | null (thd3 sp) = fst3 sp ++ [makeTravelled (snd3 sp)]
                            | isObstacle $ head (thd3 sp) = fst3 sp ++ rotateTile (snd3 sp) : thd3 sp
                            | otherwise = fst3 sp ++ makeTravelled (snd3 sp) : makeGuard (head . thd3 $ sp) (getGuardDirection $ snd3 sp) : (tail . thd3) sp

getFinalBoard :: [[Tile]] -> [[Tile]]
getFinalBoard m = step.last $ takeWhile (any containsGuard) $ iterate step m

countTraversedSpots :: [[Tile]] -> Int
countTraversedSpots = sum . map (foldl (\acc t -> if isTravelled t then acc + 1 else acc) 0)

isIntersection :: Tile -> Bool
isIntersection (Travelled, h) = length h > 1
isIntersection _ = False

getIntersections :: [[Tile]] -> [Tile]
getIntersections = concatMap (filter isIntersection)

isInLoop :: [[Tile]] -> Bool
isInLoop m = True

main = do
    contents <- lines <$> readFile "inputs/day6-test.txt"
    print $ (getIntersections.getFinalBoard.stringsToTiles) contents
    --print $ (length.filter isLoopableIntersection . getIntersections.getFinalBoard.stringsToTiles) contents
