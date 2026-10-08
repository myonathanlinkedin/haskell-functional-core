module Core (
    subsetConvolutionMin,
    joinOrderMinCost,
    popCount,
    ) where

import Data.Bits
import Data.Array
import Data.List (foldl')
import Data.Maybe (fromMaybe)

type Subset = Int

-- | Compute the min-sum subset convolution of two functions f and g defined on subsets of an n-element set.
--   The result is an array h where
--     h[S] = min_{A ⊆ S} (f[A] + g[S \\ A])
--   The functions f and g are given as arrays indexed by subset bitmask.
subsetConvolutionMin :: Int -> Array Subset Int -> Array Subset Int -> Array Subset Int
subsetConvolutionMin n f g = listArray bounds [conv s | s <- range bounds]
  where
    size = 1 `shiftL` n
    bounds = (0, size - 1)
    maxVal = maxBound :: Int

    conv :: Subset -> Int
    conv s = foldl' min maxVal candidates
      where
        -- iterate over all subsets A of s (including empty)
        candidates = [ f ! a + g ! (s `xor` a) | a <- subsets s ]

    -- generate all subsets of a given mask (including 0)
    subsets :: Subset -> [Subset]
    subsets 0 = [0]
    subsets mask = go mask []
      where
        go 0 acc = 0 : acc
        go m acc = go ((m - 1) .&. mask) (m : acc)

-- | Given a list of table cardinalities, compute the minimum total cost of joining all tables.
--   The cost model assumes joining two subsets A and B costs (size A) * (size B),
--   where size X is the sum of cardinalities of tables in X.
--   The algorithm uses subset DP and runs in O(3^n) time.
joinOrderMinCost :: [Int] -> Int
joinOrderMinCost sizes = dp ! fullMask
  where
    n = length sizes
    fullMask = (1 `shiftL` n) - 1
    bounds = (0, fullMask)

    -- precompute sum of sizes for each subset
    sumSize :: Array Subset Int
    sumSize = listArray bounds [ subsetSum s | s <- range bounds ]

    subsetSum :: Subset -> Int
    subsetSum s = foldl' (+) 0 [ sizes !! i | i <- [0..n-1], testBit s i ]

    -- dp[S] = minimal cost to join all tables in subset S
    dp :: Array Subset Int
    dp = listArray bounds [ cost s | s <- range bounds ]

    cost :: Subset -> Int
    cost 0 = 0
    cost s
      | popCount s == 1 = 0   -- single table, no join needed
      | otherwise = minimum [ dp ! a + dp ! b + sumSize ! a * sumSize ! b
                            | a <- properNonEmptySubsets s
                            , let b = s `xor` a ]

    -- generate all proper non‑empty subsets of a mask
    properNonEmptySubsets :: Subset -> [Subset]
    properNonEmptySubsets mask = filter (\x -> x /= 0 && x /= mask) (subsets mask)

    subsets :: Subset -> [Subset]
    subsets 0 = [0]
    subsets mask = go mask []
      where
        go 0 acc = 0 : acc
        go m acc = go ((m - 1) .&. mask) (m : acc)

-- | Simple popCount using Bits (available in base)
popCount :: Bits a => a -> Int
popCount = popCount' 0
  where
    popCount' c 0 = c
    popCount' c x = popCount' (c + 1) (x .&. (x - 1))
