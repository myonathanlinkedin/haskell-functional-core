module Core (HLL, newHLL, hllAdd, hllCount, splitmix64) where

import Data.Bits
import Data.Word
import Data.Array.Unboxed
import Data.Array.Base (unsafeAt)
import Data.Array (Array, listArray, (!), (//))

-- | HyperLogLog data structure
data HLL = HLL
  { precision :: !Int               -- ^ Number of bits for register index (p)
  , registers :: !(UArray Int Word8) -- ^ Registers storing max rank
  } deriving (Eq, Show)

-- | Create an empty HLL with given precision (4 <= p <= 16)
newHLL :: Int -> HLL
newHLL p
  | p < 4 || p > 16 = error "precision must be between 4 and 16"
  | otherwise       = HLL p (listArray (0, m-1) (repeat 0))
  where
    m = 1 `shiftL` p   -- number of registers

-- | Add a hashed element (64‑bit) to the HLL, returning a new HLL.
hllAdd :: Word64 -> HLL -> HLL
hllAdd w hll@(HLL p regs) = hll { registers = regs // [(idx, newRank)] }
  where
    m = 1 `shiftL` p
    idx = fromIntegral $ (w `shiftR` (64 - p)) .&. (fromIntegral (m-1))
    -- shift left to drop index bits, then count leading zeros
    w' = w `shiftL` p
    rank = 1 + (countLeadingZeros w' - p)
    cur  = regs `unsafeAt` idx
    newRank = if rank > cur then fromIntegral rank else cur

-- | Estimate cardinality from the HLL.
hllCount :: HLL -> Double
hllCount (HLL p regs) = finalEstimate
  where
    m = fromIntegral (1 `shiftL` p) :: Double
    -- harmonic sum of 2^{-M[i]}
    z = 1.0 / (sum [ 2 ** (negate (fromIntegral (regs ! i))) | i <- indices regs ])
    -- raw estimate
    alpha
      | m == 16   = 0.673
      | m == 32   = 0.697
      | m == 64   = 0.709
      | otherwise = 0.7213 / (1.0 + 1.079 / m)
    raw = alpha * m * m * z
    v = fromIntegral $ length [ () | i <- indices regs, regs ! i == 0 ]
    -- small range correction (linear counting)
    smallCorr
      | raw <= (5.0/2.0) * m && v /= 0 = m * log (m / v)
      | otherwise                     = raw
    -- large range correction
    two64 = 18446744073709551616.0 :: Double
    finalEstimate
      | smallCorr > (1.0/30.0) * two64 = -two64 * log (1.0 - smallCorr / two64)
      | otherwise                     = smallCorr

-- | Pure 64‑bit splitmix64 hash (used for test data)
splitmix64 :: Word64 -> Word64
splitmix64 x0 = z3
  where
    z1 = (x0 + 0x9e3779b97f4a7c15) :: Word64
    z2 = (z1 `xor` (z1 `shiftR` 30)) * 0xbf58476d1ce4e5b9
    z3 = (z2 `xor` (z2 `shiftR` 27)) * 0x94d049bb133111eb
    z4 = z3 `xor` (z3 `shiftR` 31)
-}
