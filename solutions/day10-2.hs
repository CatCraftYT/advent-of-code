module Main where
import Data.Array
import Data.List
import Data.Bifunctor
import Data.Ord
import Data.Ratio
import Data.Either

-- Index is (i,j), matrix array is flat
type Matrix = Array (Int,Int) Rational

testMatrix :: Matrix
testMatrix = array ((0,0),(2,2)) [((0,0),1),((0,1),2),((0,2),3),((1,0),4),((1,1),5),((1,2),6),((2,0),7),((2,1),8),((2,2),9)]

zeroMatrix :: Int -> Int -> Matrix
zeroMatrix m n = array ((0,0),(m-1,n-1)) . concat $ [[((i,j),0) | j <- [0..n-1]] | i <- [0..m-1]]

rows :: Array (Int,Int) a -> [[((Int,Int), a)]]
rows = groupBy (\a b -> (fst.fst) a == (fst.fst) b) . assocs

listToVector :: [Rational] -> Matrix
listToVector l = listArray ((0,0), (length l - 1,0)) l

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

matadd :: (Num a, Eq i, Ix i) => Array (i,i) a -> Array (i,i) a -> Array (i,i) a
matadd m1 m2
  | bounds m1 /= bounds m2 = error "Incompatible matrix sizes"
  | otherwise = m1 // zipWith (\((i,j), x1) x2 -> ((i,j), x1+x2)) (assocs m1) (elems m2)

-- A mess of a function but it does quite prettily print a matrix
showMatrix :: Show a => Array (Int,Int) a -> IO ()
showMatrix = putStrLn . (++"|") . intercalate "|\n" . map (('|':) . intercalate "  ") . transpose . map ((\col -> map (padToLength (maxLength col)) col) . map (show . snd)) . transpose . rows
  where maxLength = maximum . map length
        padToLength l s = replicate (l - length s) ' ' ++ s

swapRows :: Int -> Int -> Matrix -> Matrix
swapRows i j m = m // map (\((_,k),x) -> ((j,k),x)) iRow // map (\((_,k),x) -> ((i,k),x)) jRow
  where iRow = filter ((==i) . fst . fst) . assocs $ m
        jRow = filter ((==j) . fst . fst) . assocs $ m
addRow :: Rational -> Int -> Int -> Matrix -> Matrix
addRow mult i j m = m // zipWith (\ix -> second (+(ix*mult))) iRow jRow
  where iRow = map snd . filter ((==i) . fst . fst) . assocs $ m
        jRow = filter ((==j) . fst . fst) . assocs $ m
mulRow :: Rational -> Int -> Matrix -> Matrix
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

-- Solves a linear system by finding the pivot and free columns,
-- going through each row, getting all of the free variable coefficients,
-- and putting them in the solution matrix row corresponding to the pivot
-- on that row.
-- Returns the solution vectors as matrix columns.
--  - Left if there is a single solution (nx1 matrix)
--  - Right if there are infinitely many solutions. The tuple contains the
--    solution matrix and the constant column.
solutions :: Matrix -> Either Matrix (Matrix,Matrix)
solutions matrix
  | null freeVariables = Left (listToVector constantCol)
  | otherwise = Right
                (solutionMatrix
                // zipWith (\var v -> ((var,v),1)) freeVariables [0..]   -- 1s in free variable places
                // concat (zipWith (\pivot row -> zipWith (\var v -> ((pivot,v), -(reduced!(row,var)))) freeVariables [0..]) pivots [0..])   -- free variable coefficients
                ,zeroMatrix nVariables 1
                // zipWith (\pivot c -> ((pivot, 0), c)) pivots constantCol)
  where reduced = rref matrix
        (_,(_,nVariables)) = bounds matrix
        pivots = map (snd . fst . head) . filter ((==1) . sum . map (abs . snd)) . transpose $ coefficientRows
        freeVariables = map (snd . fst . head) . filter ((>1) . sum . map (abs . snd)) . transpose $ coefficientRows
        coefficientRows = map init . rows $ reduced
        constantCol = map (snd . last) . rows $ reduced
        solutionMatrix = zeroMatrix nVariables (length freeVariables)

-- Multiply by the LCM of all denominators to
-- turn every fraction into an integer
-- integerize :: Matrix -> Matrix
-- integerize m = fmap (*mlcm) m
--   where mlcm = (% 1) . foldr1 lcm . fmap denominator $ m

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
                         // [((i, nButtons+1), row) | (row, i) <- zip joltages [0..]]
          where (_, postLights) = break (==' ') line
                (buttons, postButtons) = first (map (map read . splitOn ',') . splitOn ' ' . init . filter (`notElem` "()")) . break (=='{') . tail $ postLights
                joltages = map ((% 1) . read) . splitOn ',' . filter (`notElem` "{} ") $ postButtons
                buttonCols = map buttonToList buttons
                nJoltages = length joltages
                nButtons = length buttons
                matrix = zeroMatrix nJoltages (nButtons + 2)
                buttonToList b = map (\i -> if i `elem` b then 1 else 0) [0..nJoltages-1]

solve :: [String] -> Integer
solve = sum . map solveMatrix . parseInput
  where isRound x = denominator x == 1
        solveMatrix :: Matrix -> Integer
        solveMatrix matrix
          | isLeft solutionMatrix = sum . fmap numerator . fromLeft undefined $ solutionMatrix
          | not (any (all (>=0)) solutionSpace) = error (show matrix)
          | otherwise = minimum . map sum . filter (all (>=0)) $ solutionSpace
          where (_,(_,n)) = bounds freeSolutionMatrix
                solutionMatrix = solutions matrix
                (freeSolutionMatrix, constCol) = second (fmap numerator) $ fromRight undefined solutionMatrix
                solutionSpace = map (((`matadd` constCol) . fmap numerator) . matmul freeSolutionMatrix) points
                points = map (listToVector . map fromIntegral) $ genPoints (n+1) 100

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day10-test.txt"
  print (solve contents)
