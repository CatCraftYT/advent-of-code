import Data.Map.Strict qualified as M
import Data.Map.Strict ( Map )
import Data.List.Extra ( find, splitOn )
import Data.Maybe ( isNothing, fromJust, fromMaybe )

data Side = West | East deriving Eq
data Tile = Wall | Box Side | Robot | Empty deriving Eq
type Position = (Int,Int)
type Direction = (Int,Int)
type Board = Map Position Tile

isBox :: Tile -> Bool
isBox (Box _) = True
isBox _ = False

getBoxSide :: Tile -> Side
getBoxSide (Box s) = s

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
            | c == '[' = Box West
            | c == ']' = Box East
            | c == '@' = Robot
            | otherwise = Empty

expandStringBoard :: [String] -> [String]
expandStringBoard b = [concat [expand c | c <- row] | row <- b]
    where expand c
            | c == '#' = "##"
            | c == 'O' = "[]"
            | c == '@' = "@."
            | otherwise = ".."

moveTile :: Board -> Position -> Direction -> Board
moveTile b _ (0,0) = b
moveTile bOriginal p d = fromMaybe bOriginal $ moveTile' p d bOriginal
    where moveTile' (x,y) (dx,dy) b
            | nextTile == Empty = Just $ swap (x,y) (x+dx,y+dy) b
            | nextTile == Wall = Nothing
            | dy /= 0 && isNothing pushDouble = Nothing
            | dy /= 0 = swap (x,y) (x+dx,y+dy) <$> pushDouble
            | isNothing pushedDirect = Nothing
            | otherwise = swap (x,y) (x+dx,y+dy) <$> pushedDirect
            where nextTile = fromJust (M.lookup (x+dx,y+dy) b)
                  pushedDirect = moveTile' (x+dx,y+dy) (dx,dy) b
                  pushDouble = moveTile' (x+dx,y+dy) (dx,dy) b >>= moveTile' boxOtherSide (dx,dy)
                  boxOtherSide
                    | getBoxSide nextTile == East = (x+dx-1,y+dy)
                    | otherwise = (x+dx+1,y+dy)

updateBoard :: Board -> Direction -> Board
updateBoard b = moveTile b robotPosition
    where robotPosition = fst.fromJust.find ((==Robot).snd) . M.assocs $ b

getGPSNumber :: Position -> Int
getGPSNumber (x,y) = y * 100 + x

getGPSBoxSum :: Board -> Int
getGPSBoxSum = sum.map (getGPSNumber.fst) . filter (isEastFacing.snd) . M.assocs
    where isEastFacing t
            | not.isBox $ t = False
            | getBoxSide t == West = True
            | otherwise = False

main = do
    [board,movements] <- splitOn "\n\n" <$> readFile "inputs/day15.txt"
    print $ getGPSBoxSum . foldl updateBoard (parseBoard.expandStringBoard.lines $ board) $ map arrowToDirection . filter (/='\n') $ movements
