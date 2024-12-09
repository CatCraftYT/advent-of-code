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
subVector2D v1 v2 = addVector2D v1 (scalarMulVec2D (negate 1) v2)

scalarMulVec2D :: Int -> Vector2D -> Vector2D
scalarMulVec2D n = bimap (n *) (n *)

vectorBetween :: Vector2D -> Vector2D -> Vector2D
vectorBetween v1 v2 = v2 `subVector2D` v1

--------------------------------------------------

parseAntennaMap :: [String] -> Map Char [Vector2D]
parseAntennaMap m = Map.fromListWith (++) $ concat [[(c, [makeVector2D i j]) | (c,j) <- zip row [0..], c /= head "."] | (row,i) <- zip m [0..]]

getAntinodes :: Vector2D -> Vector2D -> [Vector2D]
getAntinodes p1 p2 = [p1 `subVector2D` d, p2 `addVector2D` d]
    where d = vectorBetween p1 p2

getAllAntinodes :: Map Char [Vector2D] -> Map Char (Set Vector2D)
getAllAntinodes = Map.map (Set.fromList.concatMap (uncurry getAntinodes) . getAntennaCombos)
    where getAntennaCombos xs = filter (uncurry (/=)) ((,) <$> xs <*> xs)

getUniqueAntinodes :: Map Char (Set Vector2D) -> Set Vector2D
getUniqueAntinodes m = Set.unions $ Map.elems m

predicateWithin :: Int -> Int -> Vector2D -> Bool
predicateWithin x y v = (vx >= 0 && vx < x) && (vy >= 0 && vy < y)
    where vx = fst v
          vy = snd v

main = do
    contents <- lines <$> readFile "inputs/day8.txt"
    let (boundX, boundY) = (length.head $ contents, length contents)
    print $ length . Set.filter (predicateWithin boundX boundY) . getUniqueAntinodes.getAllAntinodes.parseAntennaMap $ contents
