module Engine (subsetSumExists) where

import Types (IntSet)
import Data.List (sort)

-- Compute all possible subset sums of a list.
subsetSums :: IntSet -> [Int]
subsetSums = foldr (\x acc -> acc ++ map (x +) acc) [0]

-- Binary search for a value in a sorted list.
binarySearch :: Int -> [Int] -> Bool
binarySearch _ [] = False
binarySearch target xs = go 0 (length xs - 1)
  where
    go lo hi
      | lo > hi = False
      | otherwise =
          let mid = (lo + hi) `div` 2
              midVal = xs !! mid
           in case compare target midVal of
                LT -> go lo (mid - 1)
                GT -> go (mid + 1) hi
                EQ -> True

-- Meet-in-the-middle subset sum existence check.
subsetSumExists :: IntSet -> Int -> Bool
subsetSumExists xs target =
  let (as, bs) = splitAt (length xs `div` 2) xs
      sumsA = subsetSums as
      sumsB = sort (subsetSums bs)
   in any (\s -> binarySearch (target - s) sumsB) sumsA
