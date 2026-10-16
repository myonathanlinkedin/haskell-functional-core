module Types where

import System.Random (StdGen)

-- | A graph represented as an adjacency list.
--   The list index corresponds to the vertex id.
type Graph = [[Int]]

-- | A coloring is a list of colors, one per vertex.
type Coloring = [Int]

-- | Parameters for the simulated annealing algorithm.
--   (initial temperature, cooling rate, minimum temperature, maximum iterations)
type SAParams = (Double, Double, Double, Int)

-- | Random number generator state.
type RNG = StdGen
