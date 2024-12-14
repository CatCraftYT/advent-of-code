import Data.Bifunctor ( bimap )

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

countQuadrants :: [Robot] -> [Int]
countQuadrants rs = [length q1, length q2, length q3, length q4]
    where halfWidth = mapWidth `div` 2
          halfHeight = mapHeight `div` 2
          q1 = filter (\r -> (fst.position) r < halfWidth && (snd.position) r < halfHeight) rs
          q2 = filter (\r -> (fst.position) r > halfWidth && (snd.position) r < halfHeight) rs
          q3 = filter (\r -> (fst.position) r < halfWidth && (snd.position) r > halfHeight) rs
          q4 = filter (\r -> (fst.position) r > halfWidth && (snd.position) r > halfHeight) rs

main = do
    contents <- lines <$> readFile "inputs/day14.txt"
    print $ product . countQuadrants . last . take 101 . iterate updateRobots . parseInput $ contents
