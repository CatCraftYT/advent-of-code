import Data.Map.Strict qualified as M
import Data.Map.Strict ( Map )
import Data.List.Extra ( find, splitOn )
import Data.Maybe ( isNothing, fromJust, fromMaybe )

data Tile = Wall | Box | Robot | Empty deriving Eq
type Position = (Int,Int)
type Direction = (Int,Int)
type Board = Map Position Tile

swap :: Ord k => k -> k -> Map k a -> Map k a
swap k1 k2 m = M.update (const $ k2 `M.lookup` m) k1 . M.update (const $ k1 `M.lookup` m) k2 $ m

arrowToDirection :: Char -> Direction
arrowToDirection c
    | c == '^' = (0,-1)
    | c == 'v' = (0,1)
    | c == '>' = (1,0)
    | c == '<' = (-1,0)
    | otherwise = (0,0)

parseBoard :: [String] -> Board
parseBoard m = M.fromList $ concat [[((x,y), stringToTile c) | (x,c) <- zip [0..] row] | (y,row) <- zip [0..] m]
    where stringToTile c
            | c == '#' = Wall
            | c == 'O' = Box
            | c == '@' = Robot
            | otherwise = Empty

moveTile :: Board -> Position -> Direction -> Board
moveTile b _ (0,0) = b
moveTile b p d = fromMaybe b $ moveTile' p d
    where moveTile' (x,y) (dx,dy)
            | nextTile == Empty = Just $ swap (x,y) (x+dx,y+dy) b
            | nextTile == Wall = Nothing
            | isNothing pushed = Nothing
            | otherwise = Just $ swap (x,y) (x+dx,y+dy) (fromJust pushed)
            where nextTile = fromJust (M.lookup (x+dx,y+dy) b)
                  pushed = moveTile' (x+dx,y+dy) (dx,dy)

updateBoard :: Board -> Direction -> Board
updateBoard b = moveTile b robotPosition
    where robotPosition = fst.fromJust.find ((==Robot).snd) . M.assocs $ b

getGPSNumber :: Position -> Int
getGPSNumber (x,y) = y * 100 + x

getGPSBoxSum :: Board -> Int
getGPSBoxSum = sum.map (getGPSNumber.fst) . filter ((==Box).snd) . M.assocs

main = do
    [board,movements] <- splitOn "\n\n" <$> readFile "inputs/day15.txt"
    print $ getGPSBoxSum . foldl updateBoard (parseBoard.lines $ board) $ map arrowToDirection . filter (/='\n') $ movements
