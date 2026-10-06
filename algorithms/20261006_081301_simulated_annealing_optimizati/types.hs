module Types where

import System.Random (StdGen)

-- | Undirected weighted graph represented as a symmetric distance matrix.
--   The array indices are (i,j) where i and j are vertex identifiers.
type Graph = Array (Int, Int) Double

-- | A candidate solution: a permutation of vertex identifiers representing a tour.
type Path = [Int]

-- | Parameters controlling the simulated annealing schedule.
data SAParams = SAParams
  { initialTemp :: Double   -- ^ Starting temperature.
  , finalTemp   :: Double   -- ^ Temperature at which the algorithm stops.
  , alpha       :: Double   -- ^ Multiplicative cooling factor (0 < alpha < 1).
  , innerIter   :: Int      -- ^ Number of candidate evaluations per temperature.
  } deriving (Show, Eq)

-- | Encapsulates the random generator state used throughout the algorithm.
type RNG = StdGen
