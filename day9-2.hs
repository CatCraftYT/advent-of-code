import Data.Bifunctor ( second )

data DiskChunk = DiskChunk {chunkId :: Int, usedSpace :: Int, freeSpace :: Int} deriving (Show, Eq)

parseDiskMap :: String -> [DiskChunk]
parseDiskMap = parseDiskMap' 0
    where parseDiskMap' depth [x] = [DiskChunk depth (read [x]) 0]
          parseDiskMap' depth (x:y:ys) = DiskChunk depth (read [x]) (read [y]) : parseDiskMap' (depth + 1) ys

addFreeSpaceToEnd :: Int -> [DiskChunk] -> [DiskChunk]
addFreeSpaceToEnd n cs = init cs ++ [DiskChunk cid used (free + n)]
    where (DiskChunk cid used free) = last cs

insertAtFree :: DiskChunk -> [DiskChunk] -> (Bool, [DiskChunk])
insertAtFree _ [] = (False, [])
insertAtFree (DiskChunk cid used free) (c:cs)
    | used <= cFree = (True, DiskChunk cCid cUsed 0 : DiskChunk cid used (cFree - used) : cs)
    | otherwise = second (c :) $ insertAtFree (DiskChunk cid used free) cs
    where cFree = freeSpace c
          cUsed = usedSpace c
          cCid = chunkId c

compactDisk :: [DiskChunk] -> [DiskChunk]
compactDisk = reverse . compactDisk' . reverse
    where compactDisk' [] = []
          compactDisk' (c:cs)
              | fst result = compactDisk' (prevWithGap : (tail.snd) result)
              | otherwise = c : compactDisk' cs
              where result = second reverse $ insertAtFree c (reverse cs)
                    (DiskChunk cid used free) = head.snd $ result
                    prevWithGap = DiskChunk cid used (free + usedSpace c + freeSpace c)

checksum :: [DiskChunk] -> Int
checksum d = sum [n * x | (n,x) <- zip [0..] $ convertToNumbers d]
    where convertToNumbers = concatMap (\c -> replicate (usedSpace c) (chunkId c) ++ replicate (freeSpace c) 0)

main = do
    contents <- head . lines <$> readFile "inputs/day9.txt"
    print $ checksum.compactDisk.parseDiskMap $ contents