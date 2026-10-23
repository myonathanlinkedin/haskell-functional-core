module Core (
    CuckooFilter,
    emptyFilter,
    insert,
    delete,
    member
) where

import Data.Array (Array, listArray, (!), (//))
import Data.Bits (shiftR, (.&.), xor)
import Data.Word (Word64, Word16)
import Data.Maybe (listToMaybe)

-- | Configuration constants
bucketSize :: Int
bucketSize = 4          -- slots per bucket

maxKicksDefault :: Int
maxKicksDefault = 500  -- maximum relocation attempts

fingerprintBits :: Int
fingerprintBits = 8    -- size of fingerprint in bits

-- | The cuckoo filter data structure (pure)
data CuckooFilter = CuckooFilter {
    buckets      :: Array Int [Word16], -- each bucket holds up to bucketSize fingerprints
    mask         :: Int,                -- bucketCount - 1, assuming power‑of‑two size
    maxKicks     :: Int                 -- relocation limit
} deriving (Show, Eq)

-- | Create an empty filter with a given power‑of‑two bucket count.
emptyFilter :: Int -> CuckooFilter
emptyFilter bucketCountPow2
    | bucketCountPow2 <= 0 = error "bucket count must be positive"
    | bucketCountPow2 .&. (bucketCountPow2 - 1) /= 0 = error "bucket count must be a power of two"
    | otherwise = CuckooFilter {
        buckets = listArray (0, bucketCountPow2 - 1) (repeat []),
        mask    = bucketCountPow2 - 1,
        maxKicks = maxKicksDefault
    }

-- | Primary hash: a simple multiplicative hash (Knuth's constant)
hash64 :: Word64 -> Word64
hash64 x = x * 0x9e3779b97f4a7c15

-- | Compute a non‑zero fingerprint from a key.
fingerprint :: Word64 -> Word16
fingerprint key =
    let raw = fromIntegral (hash64 (key `xor` 0xdeadbeefdeadbeef) .&. fpMask)
        fp  = if raw == 0 then 1 else raw
    in fp
  where
    fpMask = (1 `shiftL` fingerprintBits) - 1

-- | Index of the first bucket.
index1 :: Word64 -> Int -> Int
index1 key m = fromIntegral (hash64 key) .&. m

-- | Alternate index given a bucket and a fingerprint.
altIndex :: Int -> Word16 -> Int -> Int
altIndex i fp m = (i `xor` fromIntegral (hash64 (fromIntegral fp))) .&. m

-- | Insert a key; returns Nothing on failure, otherwise the updated filter.
insert :: Word64 -> CuckooFilter -> Maybe CuckooFilter
insert key cf = go fp i1 i2 0 cf
  where
    m   = mask cf
    fp  = fingerprint key
    i1  = index1 key m
    i2  = altIndex i1 fp m

    go :: Word16 -> Int -> Int -> Int -> CuckooFilter -> Maybe CuckooFilter
    go curFp idxA idxB kicks f
        | kicks > maxKicks f = Nothing
        | hasSpace (buckets f ! idxA) = Just $ addToBucket f idxA curFp
        | hasSpace (buckets f ! idxB) = Just $ addToBucket f idxB curFp
        | otherwise =
            let (chosenIdx, otherIdx) = if even kicks then (idxA, idxB) else (idxB, idxA)
                bucket = buckets f ! chosenIdx
                evicted = head bucket
                newBucket = curFp : tail bucket
                f' = f { buckets = buckets f // [(chosenIdx, newBucket)] }
                nextIdx = altIndex otherIdx evicted (mask f')
            in go evicted otherIdx nextIdx (kicks + 1) f'

-- | Delete a key; always succeeds (no effect if absent).
delete :: Word64 -> CuckooFilter -> CuckooFilter
delete key cf = removeFromBucket (removeFromBucket cf i1 fp) i2 fp
  where
    m  = mask cf
    fp = fingerprint key
    i1 = index1 key m
    i2 = altIndex i1 fp m

-- | Membership test.
member :: Word64 -> CuckooFilter -> Bool
member key cf = fp `elem` (buckets cf ! i1) || fp `elem` (buckets cf ! i2)
  where
    m  = mask cf
    fp = fingerprint key
    i1 = index1 key m
    i2 = altIndex i1 fp m

-- Helper: check if a bucket has free slot.
hasSpace :: [Word16] -> Bool
hasSpace bucket = length bucket < bucketSize

-- Helper: add fingerprint to a bucket (assumes space).
addToBucket :: CuckooFilter -> Int -> Word16 -> CuckooFilter
addToBucket cf idx fp = cf { buckets = buckets cf // [(idx, fp : buckets cf ! idx)] }

-- Helper: remove first occurrence of a fingerprint from a bucket.
removeFromBucket :: CuckooFilter -> Int -> Word16 -> CuckooFilter
removeFromBucket cf idx fp = cf { buckets = buckets cf // [(idx, filter (/= fp) (buckets cf ! idx))] }
