module Core (
    CMS,
    emptyCMS,
    add,
    estimate,
    merge,
    decay,
    -- Exported for testing
    hashWithSeed
) where

import Data.Bits (xor, shiftR, shiftL, (.&.))
import Data.Word (Word64)
import Data.List (transpose)

-- | Count-Min Sketch data structure with exponential decay capability.
data CMS = CMS
    { width     :: !Int          -- ^ Number of columns (hash range)
    , depth     :: !Int          -- ^ Number of rows (hash functions)
    , hashSeeds :: ![Word64]     -- ^ Distinct seeds for each hash function
    , table     :: ![[Int]]      -- ^ 2‑D counter matrix (depth × width)
    } deriving (Eq, Show)

-- | Create an empty CMS given desired width and depth.
emptyCMS :: Int -> Int -> CMS
emptyCMS w d
    | w <= 0 || d <= 0 = error "emptyCMS: width and depth must be positive"
    | otherwise        = CMS w d seeds (replicate d (replicate w 0))
  where
    -- Simple deterministic seeds (first d odd numbers)
    seeds = take d $ iterate (+2) 0x9e3779b97f4a7c15

-- | Add an element to the sketch, returning a new sketch.
add :: (Hashable a) => a -> CMS -> CMS
add x cms@(CMS w d seeds tbl) = cms { table = zipWith updateRow seeds tbl }
  where
    updateRow seed row = zipWith incAtIdx [0..] row
      where
        idx = hashWithSeed x seed w
        incAtIdx i cnt = if i == idx then cnt + 1 else cnt

-- | Estimate the frequency of an element (upper bound).
estimate :: (Hashable a) => a -> CMS -> Int
estimate x (CMS w _ seeds tbl) = minimum $ zipWith idxCount seeds tbl
  where
    idxCount seed row = row !! hashWithSeed x seed w

-- | Merge two sketches with identical dimensions.
merge :: CMS -> CMS -> CMS
merge (CMS w1 d1 seeds1 tbl1) (CMS w2 d2 seeds2 tbl2)
    | w1 /= w2 || d1 /= d2 = error "merge: dimension mismatch"
    | seeds1 /= seeds2     = error "merge: hash seeds differ"
    | otherwise            = CMS w1 d1 seeds1 (zipWith (zipWith (+)) tbl1 tbl2)

-- | Apply exponential decay to all counters.
--   factor must be in (0,1]; counters are rounded down.
decay :: Double -> CMS -> CMS
decay factor cms@(CMS w d seeds tbl)
    | factor <= 0 || factor > 1 = error "decay: factor must be in (0,1]"
    | otherwise = cms { table = map (map (floor . (* factor) . fromIntegral)) tbl }

--------------------------------------------------------------------
-- Hashing utilities
--------------------------------------------------------------------
class Hashable a where
    hashWithSeed :: a -> Word64 -> Int -> Int
    -- ^ Produce an index in [0, width) using a seed.

instance Hashable Int where
    hashWithSeed x seed w = fromIntegral $ (mix64 (fromIntegral x `xor` seed)) `mod` fromIntegral w

instance Hashable Integer where
    hashWithSeed x seed w = fromIntegral $ (mix64 (fromIntegral (x `mod` (2^64)) `xor` seed)) `mod` fromIntegral w

instance Hashable Char where
    hashWithSeed c seed w = fromIntegral $ (mix64 (fromIntegral (fromEnum c) `xor` seed)) `mod` fromIntegral w

instance Hashable String where
    hashWithSeed str seed w = fromIntegral $ (foldl mixStep seed (map (fromIntegral . fromEnum) str)) `mod` fromIntegral w
      where
        mixStep h c = mix64 (h `xor` c)

-- | 64‑bit mix function (splitmix64 variant) – pure and deterministic.
mix64 :: Word64 -> Word64
mix64 z0 = z5
  where
    z1 = (z0 + 0x9e3779b97f4a7c15) .&. 0xFFFFFFFFFFFFFFFF
    z2 = (z1 `xor` (z1 `shiftR` 30)) * 0xbf58476d1ce4e5b9 .&. 0xFFFFFFFFFFFFFFFF
    z3 = (z2 `xor` (z2 `shiftR` 27)) * 0x94d049bb133111eb .&. 0xFFFFFFFFFFFFFFFF
    z5 = z3 `xor` (z3 `shiftR` 31)

--------------------------------------------------------------------
-- End of core.hs
--------------------------------------------------------------------
