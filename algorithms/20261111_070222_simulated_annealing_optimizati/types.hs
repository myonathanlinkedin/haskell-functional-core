module Types (
    Vertex,
    Edge,
    Graph,
    Assignment,
    SAParams(..),
    initialAssignment,
    evaluate
) where

import qualified Data.IntMap.Strict as IM

type Vertex = Int
type Edge = (Vertex, Vertex)
type Graph = [Edge]

-- Mapping from vertex to Bool indicating side of cut (True / False)
type Assignment = IM.IntMap Bool

data SAParams = SAParams
  { initialTemp :: Double      -- ^ Starting temperature
  , coolingRate :: Double      -- ^ Multiplicative factor (<1) applied each temperature step
  , stepsPerTemp :: Int        -- ^ Number of neighbour proposals per temperature
  , minTemp :: Double          -- ^ Temperature at which the algorithm stops
  } deriving (Show, Eq)

-- | Create an initial assignment where every vertex is placed on the same side (False).
initialAssignment :: [Vertex] -> Assignment
initialAssignment vs = IM.fromList $ zip vs (repeat False)

-- | Evaluate the cut size of a given assignment on a graph.
evaluate :: Graph -> Assignment -> Int
evaluate g assign = length $ filter crossing g
  where
    crossing (u,v) = case (IM.lookup u assign, IM.lookup v assign) of
      (Just a, Just b) -> a /= b
      _                -> False
