import Data.List.Extra ( transpose, (!?) )
import Data.Maybe ( catMaybes )

getRows :: [String] -> [String]
getRows = id

getCols :: [String] -> [String]
getCols = transpose

getLeftDiagonals :: [String] -> [String]
getLeftDiagonals m = [catMaybes [m !! i !? (i + offset) | i <- rows] | offset <- offsets]
    where rows = [0..length m - 1]
          offsets = [(negate.length) m + 1 .. (length.head) m - 1]

getRightDiagonals :: [String] -> [String]
getRightDiagonals m = 
