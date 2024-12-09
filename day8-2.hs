import Data.Bifunctor (bimap)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Vector2D = (Int, Int)

makeVector2D :: Int -> Int -> Vector2D
makeVector2D x y = (fromIntegral x, fromIntegral y)

addVector2D :: Vector2D -> Vector2D -> Vector2D
addVector2D v1 = bimap (fst v1 +) (snd v1 +)

subVector2D :: Vector2D -> Vector2D -> Vector2D
subVector2D v1 v2 = addVector2D v1 (scalarMulVec2D v2 (negate 1))

scalarMulVec2D :: Vector2D -> Int -> Vector2D
scalarMulVec2D v n = bimap (n *) (n *) v

vectorBetween :: Vector2D -> Vector2D -> Vector2D
vectorBetween v1 v2 = v2 `subVector2D` v1

normalizeInt :: Vector2D -> Vector2D
normalizeInt (vx, vy) = (vx `div` gcd, vy `div` gcd)
    where gcd = last [d | d <- [1..max 1 $ max vx vy], vx `mod` d == 0 && vy `mod` d == 0]

--------------------------------------------------

parseAntennaMap :: [String] -> Map Char [Vector2D]
parseAntennaMap m = Map.fromListWith (++) $ concat [[(c, [makeVector2D i j]) | (c,j) <- zip row [0..], c /= head "."] | (row,i) <- zip m [0..]]

getAntinodes :: Int -> Int -> Vector2D -> Vector2D -> [Vector2D]
getAntinodes bx by p1 p2 = filter predicateWithin $ concat [[p1 `subVector2D` (d `scalarMulVec2D` mult), p2 `addVector2D` (d `scalarMulVec2D` mult)] | mult <- [1..max bx by]]
    where d = normalizeInt $ vectorBetween p1 p2
          predicateWithin (vx, vy) = (vx >= 0 && vx < bx) && (vy >= 0 && vy < by)

getAllAntinodes :: Int -> Int -> Map Char [Vector2D] -> Map Char (Set Vector2D)
getAllAntinodes bx by = Map.map (Set.fromList.concatMap (uncurry $ getAntinodes bx by) . getAntennaCombos)
    where getAntennaCombos xs = (,) <$> xs <*> xs

getUniqueAntinodes :: Map Char (Set Vector2D) -> Set Vector2D
getUniqueAntinodes m = Set.unions $ Map.elems m

main = do
    contents <- lines <$> readFile "inputs/day8.txt"
    let (boundX, boundY) = (length.head $ contents, length contents)
    print $ length . getUniqueAntinodes.getAllAntinodes boundX boundY.parseAntennaMap $ contents
