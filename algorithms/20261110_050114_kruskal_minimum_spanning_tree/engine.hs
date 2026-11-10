module Engine (kruskalMST, Edge(..), Graph) where

import Types
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Monad.ST
import Data.Array.ST
import Data.Array.MArray (MArray, newArray, readArray, writeArray)

-- | Parent array for Union‑Find (Disjoint Set Union).
type Parent s = STUArray s Int Int

-- | Rank array for Union‑Find (used for union by rank).
type Rank s = STUArray s Int Int

-- | Initialise a DSU for 'n' elements (0 … n‑1). Each element is its own parent with rank 0.
newDSU :: Int -> ST s (Parent s, Rank s)
newDSU n = do
  parent <- newArray (0, n - 1) 0
  rank   <- newArray (0, n - 1) 0
  let init i = writeArray parent i i
  mapM_ init [0 .. n - 1]
  return (parent, rank)

-- | Find the representative (root) of the set containing 'x' with path compression.
find :: Parent s -> Int -> ST s Int
find parent x = do
  p <- readArray parent x
  if p == x
    then return x
    else do
      root <- find parent p
      writeArray parent x root
      return root

-- | Union the sets containing 'x' and 'y'.
-- Returns 'True' iff a merge happened (i.e. they were previously separate).
union :: Parent s -> Rank s -> Int -> Int -> ST s Bool
union parent rank x y = do
  rx <- find parent x
  ry <- find parent y
  if rx == ry
    then return False
    else do
      rankX <- readArray rank rx
      rankY <- readArray rank ry
      case compare rankX rankY of
        LT -> writeArray parent rx ry
        GT -> writeArray parent ry rx
        EQ -> do
          writeArray parent ry rx
          writeArray rank rx (rankX + 1)
      return True

-- | Kruskal's Minimum Spanning Tree algorithm.
--   Takes the number of vertices and a list of edges.
--   Returns the list of edges belonging to the MST (in any order) and its total weight.
kruskalMST :: Int -> Graph -> ([Edge], Int)
kruskalMST n edges = runST $ do
  (parent, rank) <- newDSU n
  let sorted = sortBy (comparing weight) edges
  let go [] mst total = return (reverse mst, total)
      go (e:es) mst total = do
        merged <- union parent rank (from e) (to e)
        if merged
          then go es (e : mst) (total + weight e)
          else go es mst total
  go sorted [] 0
