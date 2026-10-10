module Core where

import Data.Bits (xor)

-- | Buddy allocator state.
data Buddy = Buddy
  { totalSize :: Int               -- ^ Total size of the memory region (must be power of two)
  , freeList  :: [(Int, Int)]      -- ^ List of free blocks as (offset, size)
  , allocated :: [(Int, Int)]      -- ^ List of allocated blocks as (offset, size)
  } deriving (Show, Eq)

-- | Initialise a buddy allocator with a given total size.
--   The size must be a power of two; otherwise the behaviour is undefined.
initBuddy :: Int -> Buddy
initBuddy sz = Buddy
  { totalSize = sz
  , freeList  = [(0, sz)]
  , allocated = []
  }

-- | Allocate a block of at least the requested size.
--   Returns the offset of the allocated block and the updated allocator,
--   or Nothing if allocation fails.
allocate :: Buddy -> Int -> Maybe (Int, Buddy)
allocate bud req
  | req <= 0 = Nothing
  | otherwise =
      let blkSize = nextPowerOfTwo req
          (maybeOff, newFree) = allocateHelper blkSize (freeList bud)
      in case maybeOff of
           Nothing   -> Nothing
           Just off  -> Just (off, bud { freeList = newFree
                                      , allocated = (off, blkSize) : allocated bud })

-- | Free a previously allocated block given its offset and size.
--   The size must be the exact block size returned by 'allocate'.
free :: Buddy -> Int -> Int -> Buddy
free bud off sz =
  let newAllocated = filter (/= (off, sz)) (allocated bud)
      newFree = coalesce (off, sz) (freeList bud)
  in bud { allocated = newAllocated, freeList = newFree }

-- ----------------------------------------------------------------------
-- Helper functions (pure)

-- | Find a free block of the exact size or split a larger one.
allocateHelper :: Int -> [(Int, Int)] -> (Maybe Int, [(Int, Int)])
allocateHelper sz free =
  case lookupExact sz free of
    Just (off, rest) -> (Just off, rest)
    Nothing ->
      case findLarger sz free of
        Nothing -> (Nothing, free)  -- no block large enough
        Just (offL, szL, restL) ->
          let half = szL `div` 2
              halves = [(offL, half), (offL + half, half)]
              newFree = halves ++ restL
          in allocateHelper sz newFree

-- | Look for a block of exactly the given size.
lookupExact :: Int -> [(Int, Int)] -> Maybe (Int, [(Int, Int)])
lookupExact _ [] = Nothing
lookupExact sz ((off, s):xs)
  | s == sz   = Just (off, xs)
  | otherwise = fmap (\(o, rest) -> (o, (off, s):rest)) (lookupExact sz xs)

-- | Find the smallest block larger than the requested size.
findLarger :: Int -> [(Int, Int)] -> Maybe (Int, Int, [(Int, Int)])
findLarger _ [] = Nothing
findLarger sz xs =
  let larger = filter (\(_, s) -> s > sz) xs
  in if null larger
        then Nothing
        else let (off, s) = minimumBySize larger
                 rest = filter (/= (off, s)) xs
             in Just (off, s, rest)

-- | Helper to pick the block with the smallest size.
minimumBySize :: [(Int, Int)] -> (Int, Int)
minimumBySize = foldr1 (\a@(_, sa) b@(_, sb) -> if sa < sb then a else b)

-- | Coalesce a freed block with its buddy if possible.
coalesce :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
coalesce (off, sz) free =
  case findBuddy (off, sz) free of
    Nothing -> (off, sz) : free
    Just (buddyOff, rest) ->
      let newOff = min off buddyOff
          newSize = sz * 2
      in coalesce (newOff, newSize) rest

-- | Find the buddy of a block in the free list.
findBuddy :: (Int, Int) -> [(Int, Int)] -> Maybe ((Int, Int), [(Int, Int)])
findBuddy (off, sz) [] = Nothing
findBuddy (off, sz) ((o, s):xs)
  | s == sz && o == (off `xor` sz) = Just ((o, s), xs)
  | otherwise = fmap (\(b, rest) -> (b, (o, s):rest)) (findBuddy (off, sz) xs)

-- | Check if a number is a power of two.
isPowerOfTwo :: Int -> Bool
isPowerOfTwo n = n > 0 && (n .&. (n - 1)) == 0
  where (.&.) = (.&.)  -- use built‑in bitwise AND from Prelude (via Num)

-- | Compute the smallest power of two >= n.
nextPowerOfTwo :: Int -> Int
nextPowerOfTwo n
  | n <= 1    = 1
  | isPowerOfTwo n = n
  | otherwise = go 1
  where
    go k
      | k >= n    = k
      | otherwise = go (k * 2)
