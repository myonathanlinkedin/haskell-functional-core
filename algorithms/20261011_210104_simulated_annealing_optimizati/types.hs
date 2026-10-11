module Types where

import Data.List (minimumBy)
import Data.Ord (comparing)

-- | Vertex identifier
type Vertex = Int

-- | Edge represented as a pair of vertices and a weight (non-negative)
type Edge = ((Vertex, Vertex), Double)

-- | Undirected weighted graph as a list of edges.
--   It is assumed that for each edge ((u,v),w) the pair (v,u) does not appear separately.
type Graph = [Edge]

-- | A tour (Hamiltonian cycle) is a permutation of vertices.
--   The first vertex is considered the start; the tour implicitly returns to the start.
type Tour = [Vertex]

-- | Compute the weight of an edge; returns Nothing if the edge does not exist.
edgeWeight :: Graph -> Vertex -> Vertex -> Maybe Double
edgeWeight g u v = lookup (ordered u v) [(ordered a b, w) | ((a,b),w) <- g]
  where
    ordered a b = if a <= b then (a,b) else (b,a)

-- | Total length of a tour. If any required edge is missing, the cost is 'infinity'.
tourCost :: Graph -> Tour -> Double
tourCost g tour = sum $ zipWith segmentCost cyclic tour
  where
    cyclic = tail tour ++ [head tour]
    segmentCost a b = case edgeWeight g a b of
                        Just w  -> w
                        Nothing -> 1/0   -- Infinity for missing edge

-- | Generate an initial random tour given a list of vertices.
initialTour :: [Vertex] -> Tour
initialTour = id   -- The caller may shuffle; kept pure for flexibility.

-- | Produce a neighboring tour by swapping two distinct positions.
neighborTour :: Tour -> Int -> Int -> Tour
neighborTour t i j
  | i == j    = t
  | otherwise = let vi = t !! i
                    vj = t !! j
                in replaceAt i vj (replaceAt j vi t)
  where
    replaceAt idx val xs = take idx xs ++ [val] ++ drop (idx+1) xs
