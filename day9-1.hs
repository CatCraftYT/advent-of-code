data DiskChunk = DiskChunk {chunkId :: Int, usedSpace :: Int, freeSpace :: Int} deriving (Show)

parseDiskMap :: String -> [DiskChunk]
parseDiskMap = parseDiskMap' 0
    where parseDiskMap' depth [x] = [DiskChunk depth (read [x]) 0]
          parseDiskMap' depth (x:y:ys) = DiskChunk depth (read [x]) (read [y]) : parseDiskMap' (depth + 1) ys

addFreeSpaceToEnd :: Int -> [DiskChunk] -> [DiskChunk]
addFreeSpaceToEnd n cs = init cs ++ [DiskChunk cid used (free + n)]
    where (DiskChunk cid used free) = last cs

-- Free space at the end is lost when using the example, don't exactly know why, doesn't matter for this
insertAtFree :: DiskChunk -> [DiskChunk] -> [DiskChunk]
insertAtFree (DiskChunk _ _ used) [] = error $ "Disk capacity exceeded by " ++ show used
insertAtFree (DiskChunk cid used free) (c:cs)
    | free > 0 = insertAtFree (DiskChunk cid used 0) (c:addFreeSpaceToEnd free cs)
    | used == 0 = c:cs
    | used <= cFree = DiskChunk cCid cUsed 0 : DiskChunk cid used (cFree - used) : cs
    | cFree > 0 = DiskChunk cCid cUsed 0 : DiskChunk cid cFree 0 : insertAtFree (DiskChunk cid diff free) cs
    | otherwise = c : insertAtFree (DiskChunk cid used free) cs
    where cFree = freeSpace c
          cUsed = usedSpace c
          cCid = chunkId c
          diff = used - cFree

isCompact :: [DiskChunk] -> Bool
isCompact = not.any (\c -> freeSpace c > 0) . init

compactDisk :: [DiskChunk] -> [DiskChunk]
compactDisk d
    | isCompact d = d
    | otherwise = compactDisk $ insertAtFree (last d) (init d)

checksum :: [DiskChunk] -> Int
checksum d = sum.snd $ foldl foldFunc (0, []) d
    where foldFunc (acc, ns) x = (acc + used, ns ++ [n * chunkId x | n <- [acc..(acc + used - 1)]])
            where used = usedSpace x

main = do
    contents <- head . lines <$> readFile "inputs/day9.txt"
    print $ checksum.compactDisk.parseDiskMap $ contents