module Types where

import System.Random (StdGen)

-- | Simple undirected weighted graph represented as an adjacency matrix.
--   The matrix is assumed symmetric and diagonal entries are zero.
type Graph = [[Double]]

-- | A solution for the Traveling Salesman Problem (TSP) is a permutation
--   of vertex indices representing the visitation order.
type Tour = [Int]

-- | Parameters controlling the simulated annealing process.
data SAParams = SAParams
  { initialTemp   :: Double   -- ^ Starting temperature
  , finalTemp     :: Double   -- ^ Temperature at which the algorithm stops
  , alpha         :: Double   -- ^ Cooling factor (0 < alpha < 1)
  , iterPerTemp   :: Int      -- ^ Number of neighbour trials per temperature
  } deriving (Show, Eq)

-- | Result of the annealing run.
data SAResult = SAResult
  { bestTour   :: Tour
  , bestCost   :: Double
  , finalTempReached :: Double
  } deriving (Show, Eq)

-- | Random generator wrapper used throughout the pure engine.
type RNG = StdGen
