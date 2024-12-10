-- Run with arguments "+RTS -N"
{-# LANGUAGE DeriveAnyClass #-}
import Data.List.Extra ( transpose, unsnoc )
import Data.Tuple.Extra (fst3, snd3, thd3)
import Data.Maybe ( fromJust, isNothing )
import Control.Parallel.Strategies ( parList, rdeepseq, using, NFData )
import GHC.Generics ( Generic )

data MovementDirection = U | R | D | L deriving (Show, Eq, Generic, NFData)
data TileState = Travelled | Obstacle | Empty | Guard MovementDirection deriving (Show, Eq, Generic, NFData)
type Tile = (TileState, [MovementDirection])
type Board = [[Tile]]

isArrowString :: Char -> Bool
isArrowString = flip elem "^>v<"

stringsToTiles :: [String] -> Board
stringsToTiles strs = [[charToTile c | c <- row] | row <- strs]
    where charToTile c
            | c == head "." = (Empty, [])
            | c == head "#" = (Obstacle, [])
            | isArrowString c = (Guard (arrowToDirection c), [])
            where arrowToDirection c
                    | c == head "^" = U
                    | c == head "v" = D
                    | c == head "<" = L
                    | c == head ">" = R

isGuard :: Tile -> Bool
isGuard (Guard _, _) = True
isGuard _ = False

isObstacle :: Tile -> Bool
isObstacle (Obstacle, _) = True
isObstacle _ = False

isTravelled :: Tile -> Bool
isTravelled (Travelled, _) = True
isTravelled _ = False

makeTravelled :: Tile -> MovementDirection -> Tile
makeTravelled (_, h) d = (Travelled, h ++ [d])

makeGuardTravelled :: Tile -> Tile
makeGuardTravelled t = makeTravelled t d
    where Guard d = fst t

makeGuard :: Tile -> MovementDirection -> Tile
makeGuard (_, h) d = (Guard d, h)

getGuard :: Board -> Tile
getGuard m = snd.head.head.filter (not.null) $ [filter fst [(isGuard col, col) | col <- row] | row <- m]

getMovementDirection :: Board -> MovementDirection
getMovementDirection m = getGuardDirection $ getGuard m
    where getGuardDirection (Guard d, _) = d

-- Assumes the element is in the list (crash otherwise)
splitOnPredicate :: (a -> Bool) -> [a] -> ([a],a,[a])
splitOnPredicate p l = (fst s, head.snd $ s, tail.snd $ s)
    where s = break p l

containsGuard :: [Tile] -> Bool
containsGuard = any isGuard

rotate :: MovementDirection -> MovementDirection
rotate d
    | d == U = R
    | d == R = D
    | d == D = L
    | d == L = U

-- Stupidly fast implementation compared to part 1's step function
step :: Board -> Board
step m
    | dir == U = transpose . map reverse . step' . map reverse . transpose $ m 
    | dir == D = transpose . step' . transpose $ m
    | dir == L = map reverse . step' . map reverse $ m
    | dir == R = step' m
    where dir = getMovementDirection m
          step' (r:rs)
            | containsGuard r = (bg ++ moveAlongRow (g:ag)) : rs
            | otherwise = r : step' rs
            where (bg, g, ag) = splitOnPredicate isGuard r
                  moveAlongRow [] = []
                  moveAlongRow [t] = [makeTravelled t dir]
                  moveAlongRow (t:n:ts)
                    | isObstacle n = makeGuard t (rotate dir) : n : ts
                    | otherwise = makeTravelled t dir : moveAlongRow (n:ts)

getFinalBoard :: Board -> (Bool, Board)
getFinalBoard m
    | isNothing finalBoard = (True, m)
    | (not.any containsGuard) finalBoardUnwrap = (False, finalBoardUnwrap)
    | otherwise = (True, finalBoardUnwrap)
    where finalBoard = unsnoc . takeWhile (\b -> any containsGuard b && (not.isInLoop) b) $ iterate step m
          finalBoardUnwrap = step.snd.fromJust $ finalBoard

nthTileToObstacle :: Board -> Int -> Board
nthTileToObstacle [r] n = [(init.fst) sp ++ (Obstacle, []) : snd sp]
    where sp = splitAt n r
nthTileToObstacle (r:rs) n
    | n <= nTraversals = ((init.fst) sp ++ (Obstacle, []) : snd sp) : rs
    | otherwise = r : nthTileToObstacle rs (n - nTraversals)
    where nTraversals = length r
          sp = splitAt n r
          t = last.fst $ sp

getObstaclePossibilities :: Board -> [Board]
getObstaclePossibilities m = filter (\b -> any (any isGuard) b && b /= m) [nthTileToObstacle m n | n <- [1..length m * (length.head) m]]

-- This is a "work harder not smarter" solution
getLoopingObstacles :: Board -> [Board]
getLoopingObstacles m = map snd $ filter fst (map getFinalBoard (getObstaclePossibilities m) `using` parList rdeepseq)

isInLoop :: Board -> Bool
isInLoop m
  | null h = False
  | d `elem` h = True
  | otherwise = False
  where (Guard d, h) = getGuard m

main = do
    contents <- lines <$> readFile "inputs/day6.txt"
    print $ (length . getLoopingObstacles . stringsToTiles) contents
