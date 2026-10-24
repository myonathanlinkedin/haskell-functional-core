module Types
    ( Node
    , Graph
    , Solution
    , Cost
    , SAParams(..)
    ) where

-- | A node identifier in the graph.
type Node = Int

-- | Weighted undirected graph represented as an adjacency matrix.
--   The matrix is square, symmetric and has zeroes on the diagonal.
type Graph = [[Double]]

-- | A candidate solution: a permutation of all nodes representing a tour.
type Solution = [Node]

-- | The total weight (cost) of a tour.
type Cost = Double

-- | Parameters controlling the simulated annealing schedule.
data SAParams = SAParams
    { initialTemp      :: Double   -- ^ Starting temperature.
    , coolingRate      :: Double   -- ^ Multiplicative factor < 1 applied each temperature step.
    , iterationsPerTemp:: Int      -- ^ Number of neighbour evaluations per temperature.
    , minTemp          :: Double   -- ^ Temperature at which the algorithm stops.
    } deriving (Show, Eq)
