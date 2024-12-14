import Data.Bifunctor ( bimap )
import Data.List ( intercalate )

data Robot = Robot {position :: (Int,Int), velocity :: (Int,Int)} deriving Show

mapWidth :: Int
mapHeight :: Int
(mapWidth, mapHeight) = (101,103)

tupleAdd :: Num a => (a,a) -> (a,a) -> (a,a)
tupleAdd (a1,b1) (a2,b2) = (a1+a2, b1+b2)

positionWraparound :: (Int,Int) -> (Int,Int)
positionWraparound (x,y)
    | x < 0 && y < 0 = (x+mapWidth, y+mapHeight)
    | x < 0 = (x+mapWidth, y `mod` mapHeight)
    | y < 0 = (x `mod` mapWidth, y+mapHeight)
    | otherwise = (x `mod` mapWidth, y `mod` mapHeight)

parseInput :: [String] -> [Robot]
parseInput = map (tupleToRobot.parseLine)
    where parseLine l = bimap (strToTuple.drop 2) (strToTuple.drop 3) $ break (==' ') l
          strToTuple s = read $ '(' : s ++ ")"
          tupleToRobot (p,v) = Robot p v

updateRobots :: [Robot] -> [Robot]
updateRobots = map updateRobot
    where updateRobot r = Robot (positionWraparound (position r `tupleAdd` velocity r)) (velocity r)

robotsToMap :: [Robot] -> String
robotsToMap rs = intercalate "\n" $ [[if or [position r == (x,y) | r <- rs] then 'R' else '.' | x <- [0..mapWidth - 1]] | y <- [0..mapHeight - 1]]

-- Question doesn't say what the christmas tree looks like, so redirect into file and look in vscode sidebar (or less)
-- The tree will show up when the horizontal and vertical 'lines' meet.
main = do
    contents <- lines <$> readFile "inputs/day14.txt"
    traverse (\(n,rs) -> putStrLn (show n ++ '\n' : robotsToMap rs ++ "\n")) $ zip [0..] . iterate updateRobots . parseInput $ contents
