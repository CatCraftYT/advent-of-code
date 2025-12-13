module Main where
import Data.Array
import Data.List
import Data.Bifunctor
import Data.Ord

-- Index is (i,j), matrix array is flat
type Matrix = Array (Int,Int) Float

testMatrix :: Matrix
testMatrix = array ((0,0),(2,2)) [((0,0),1),((0,1),2),((0,2),3),((1,0),4),((1,1),5),((1,2),6),((2,0),7),((2,1),8),((2,2),9)]

-- A mess of a function but it does quite prettily print a matrix
showMatrix :: Matrix -> IO ()
showMatrix = putStrLn . (++"|") . intercalate "|\n" . map (('|':) . intercalate "  ") . transpose . map ((\col -> map (padToLength (maxLength col)) col) . map (show . snd)) . transpose . groupBy (\a b -> (fst.fst) a == (fst.fst) b) . assocs
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
                -- by the book gives the incorrect result.
                pivotSwap = swapRows p r matrix
                scalePivot = mulRow (1/pv) r pivotSwap
                elimination = foldr id scalePivot [addRow (-(scalePivot!(r',c))) r r' | r' <- [0..m], r' /= r]


solve :: [String] -> Int
solve = const 0

main :: IO ()
main = do
  contents <- lines <$> readFile "inputs/day10.txt"
  print (solve contents)
