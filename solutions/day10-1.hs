module Main where
import Data.Array
import Data.List
import Data.Bifunctor
import Data.Ord

-- Index is (i,j), matrix array is flat
type Matrix = Array (Int,Int) Float

testMatrix :: Matrix
testMatrix = array ((0,0),(2,2)) [((0,0),1),((0,1),2),((0,2),3),((1,0),4),((1,1),5),((1,2),6),((2,0),7),((2,1),8),((2,2),9)]

zeroMatrix :: Int -> Int -> Matrix
zeroMatrix m n = array ((0,0),(m-1,n-1)) . concat $ [[((i,j),0) | j <- [0..n-1]] | i <- [0..m-1]]

rows :: Array (Int,Int) a -> [[((Int,Int), a)]]
rows = groupBy (\a b -> (fst.fst) a == (fst.fst) b) . assocs

matmul :: Matrix -> Matrix -> Matrix
matmul matrix1 matrix2
  | n1 /= m2 = error "Incompatible matrix sizes"
  | otherwise = array ((0,0),(m1,n2)) $ concat [[((i,j), dot row col) | (j, col) <- zip [0..] cols2] | (i, row) <- zip [0..] rows1]
  where (_,(m1,n1)) = bounds matrix1
        (_,(m2,n2)) = bounds matrix2
        dot :: Num a => [a] -> [a] -> a
        dot l1 = sum . zipWith (*) l1
        rows1 = map (map snd) . rows $ matrix1
        cols2 = transpose . map (map snd) . rows $ matrix2

-- A mess of a function but it does quite prettily print a matrix
showMatrix :: Show a => Array (Int,Int) a -> IO ()
showMatrix = putStrLn . (++"|") . intercalate "|\n" . map (('|':) . intercalate "  ") . transpose . map ((\col -> map (padToLength (maxLength col)) col) . map (show . snd)) . transpose . rows
  where maxLength = maximum . map length
        padToLength l s = replicate (l - length s) ' ' ++ s

swapRows :: Int -> Int -> Matrix -> Matrix
swapRows i j m = m // map (\((_,k),x) -> ((j,k),x)) iRow // map (\((_,k),x) -> ((i,k),x)) jRow
  where iRow = filter ((==i) . fst . fst) . assocs $ m
        jRow = filter ((==j) . fst . fst) . assocs $ m
addRow :: Float -> Int -> Int -> Matrix -> Matrix
addRow mult i j m = m // zipWith (\ix -> second (+(ix*mult))) iRow jRow
  where iRow = map snd . filter ((==i) . fst . fst) . assocs $ m
        jRow = filter ((==j) . fst . fst) . assocs $ m
mulRow :: Float -> Int -> Matrix -> Matrix
mulRow mult i m = m // map (second (*mult)) iRow
  where iRow = filter ((==i) . fst . fst) . assocs $ m

-- Gauss-Jordan elimination with partial pivoting
-- https://sj-graves.github.io/algorithms-book/sec-matapps-gje_proj.html
rref :: Matrix -> Matrix
rref = rref' 0 0
  where rref' :: Int -> Int -> Matrix -> Matrix
        rref' r c matrix
          | r > m || c > n = matrix
          | pv == 0 = rref' r (c+1) matrix
          | otherwise = rref' (r+1) (c+1) elimination
          where (m,n) = snd . bounds $ matrix
                -- maximumBy picks the rightmost value, so we get the highest
                -- pivot value (from max) and the lowest index value
                -- (from reverse and assoc ordering)
                ((p, _), pv) = maximumBy (comparing (abs . snd)) . reverse . filter ((==c) . snd . fst) . filter ((>= r) . fst . fst) . assocs $ matrix
                -- The above source says to do this only when pv>0,
                -- but that makes no sense... doing it all the time
                -- seems to give the correct result, and doing it
                -- by the book gives an incorrect result.
                pivotSwap = swapRows p r matrix
                scalePivot = mulRow (1/pv) r pivotSwap
                elimination = foldr id scalePivot [addRow (-(scalePivot!(r',c))) r r' | r' <- [0..m], r' /= r]

-- Solves a homogeneous linear system by finding the pivot and free columns,
-- going through each row, getting all of the free variable coefficients,
-- and putting them in the solution matrix row corresponding to the pivot
-- on that row.
-- Returns the solution vectors as matrix columns.
solutions :: Matrix -> Matrix
solutions matrix = solutionMatrix
                  // zipWith (\var v -> ((var,v),1)) freeVariables [0..]   -- 1s in free variable places
                  // concat (zipWith (\pivot row -> zipWith (\var v -> ((pivot,v), -(reduced!(row,var)))) freeVariables [0..]) pivots [0..])   -- free variable coefficients
  where reduced = rref matrix
        (_,(_,nVariables)) = bounds matrix
        pivots = map (snd . fst . head) . filter ((==1) . sum . map (abs . snd)) . transpose . rows $ reduced
        freeVariables = map (snd . fst . head) . filter ((>1) . sum . map (abs . snd)) . transpose . rows $ reduced
        solutionMatrix = zeroMatrix (nVariables+1) (length freeVariables)

genPoints :: Int -> Int -> [[Int]]
genPoints 0 _ = [[]]
genPoints dimension range1 = concatMap (\vs -> map (:vs) ints) (genPoints (dimension-1) range1)
  where ints = [-range1..range1]

-- From day 6
splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s
  | null post = [pre]
  | otherwise = pre : splitOn c (tail post)
  where (pre, post) = break (==c) s

parseInput :: [String] -> [Matrix]
parseInput = map parseLine
  where parseLine :: String -> Matrix
        parseLine line = matrix
                         // concat [[((i,j),row) | (row, i) <- zip col [0..]] | (col, j) <- zip buttonCols [0..]]
                         // [((i, nButtons), row) | (row, i) <- zip lights [0..]]
          where (lights, postLights) = first (map lightToMatrix . filter (`elem` "#.")) . break (==' ') $ line
                (buttons, _) = first (map (map read . splitOn ',') . splitOn ' ' . init . filter (`notElem` "()")) . break (=='{') . tail $ postLights
                buttonCols = map buttonToList buttons
                nLights = length lights
                nButtons = length buttons
                matrix = zeroMatrix nLights (nButtons + 1)
                lightToMatrix c
                  | c == '#' = -1.0
                  | c == '.' = -2.0
                  | otherwise = error "Unexpected char in lights"
                buttonToList b = map (\i -> if i `elem` b then 1 else 0) [0..nLights-1]

solve :: [String] -> Int
solve = sum . map solveMatrix . parseInput
  where listToVector l = listArray ((0,0), (length l - 1,0)) l
        solveMatrix :: Matrix -> Int
        solveMatrix matrix = minimum . map (sum . init . elems) . filter (not . all (==0)) $ solutionSpace
          where (_,(_,n)) = bounds solutionMatrix
                solutionMatrix = solutions matrix
                solutionSpace = filter (all (>=0)) . map (fmap ((`mod` 2) . round) . matmul solutionMatrix) $ points
                points = map (listToVector . map fromIntegral) $ genPoints (n+1) 5

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day10-test.txt"
  print (solve contents)
