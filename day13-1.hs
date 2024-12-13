import Text.Regex.TDFA
import Data.List ( transpose )
import Data.List.Extra ( splitOn )
import Data.Maybe ( mapMaybe )

type Point = (Int, Int)
type Line = (Int, Int, Int) -- Ax+Bx=C
type ClawMachine = (Line, Line)

lineA :: Line -> Int
lineA (a,_,_) = a

lineB :: Line -> Int
lineB (_,b,_) = b

lineC :: Line -> Int
lineC (_,_,c) = c

listTo3Tuple :: [a] -> (a,a,a)
listTo3Tuple [a,b,c] = (a,b,c)

intersections :: Line -> Line -> Maybe Point
intersections l1 l2
    | yDivisor == 0 || lineA l1 == 0 = Nothing
    | yRem /= 0 || xRem /= 0 = Nothing
    | xQuot < 0 || xQuot > 100 || yQuot < 0 || yQuot > 100 = Nothing
    | otherwise = Just (xQuot, yQuot)
    where yDivisor = (lineB l2 * lineA l1) - (lineB l1 * lineA l2)
          (yQuot, yRem) = quotRem (lineC l2 * lineA l1 - lineA l2 * lineC l1) yDivisor
          (xQuot, xRem) = quotRem (lineC l1 - lineB l1 * yQuot) (lineA l1)

clawMachineIntersections :: [ClawMachine] -> [Point]
clawMachineIntersections = mapMaybe (uncurry intersections)

getGroups :: String -> String -> [String]
getGroups re s = map last ((s =~ re) :: [[String]])

getXLine :: String -> Line
getXLine s = listTo3Tuple . map read $ getGroups "X[+=]([0-9]+)" s

getYLine :: String -> Line
getYLine s = listTo3Tuple . map read $ getGroups "Y[+=]([0-9]+)" s

parseInput :: [String] -> [ClawMachine]
parseInput = map (\s -> (getXLine s, getYLine s))

main = do
    contents <- splitOn "\n\n" <$> readFile "inputs/day13.txt"
    print $ sum.map (\p -> 3 * fst p + snd p) . clawMachineIntersections.parseInput $ contents
