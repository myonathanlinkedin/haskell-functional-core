module Core (
    Graph(..),
    Assignment,
    anneal,
    energy,
    randomAssignment
) where

import System.Random (StdGen, randomR)
import Data.List (foldl')
import Control.Monad (replicateM)

-- | Simple undirected graph with vertices indexed from 0 to (numVertices - 1)
data Graph = Graph
    { numVertices :: Int
    , edges       :: [(Int, Int)]   -- ^ each edge is a pair (u,v) with u < v
    } deriving (Show, Eq)

type Assignment = [Int]   -- ^ color per vertex, length == numVertices

-- | Count the number of conflicting edges (both ends share the same color)
energy :: Graph -> Assignment -> Int
energy g assign = foldl' count 0 (edges g)
  where
    count acc (u,v) = if assign !! u == assign !! v then acc + 1 else acc

-- | Produce a random initial assignment of colors
randomAssignment :: Int          -- ^ number of colors (k)
                 -> Int          -- ^ number of vertices (n)
                 -> StdGen
                 -> (Assignment, StdGen)
randomAssignment k n gen = go n gen []
  where
    go 0 g acc = (reverse acc, g)
    go m g acc =
        let (c, g') = randomR (0, k-1) g
        in go (m-1) g' (c:acc)

-- | Replace the element at index i with a new value
replaceNth :: Int -> a -> [a] -> [a]
replaceNth i x xs = let (pre, _:post) = splitAt i xs in pre ++ x : post

-- | Generate a neighbour by recoloring a single random vertex
neighbor :: Int          -- ^ number of colors (k)
         -> Graph
         -> Assignment
         -> StdGen
         -> (Assignment, StdGen)
neighbor k g assign gen =
    let (v, gen1) = randomR (0, numVertices g - 1) gen
        (c, gen2) = randomR (0, k-1) gen1
    in (replaceNth v c assign, gen2)

-- | Simulated annealing optimizer.
-- Returns the best assignment found and its conflict count.
anneal :: Graph          -- ^ problem instance
       -> Int            -- ^ number of colors (k)
       -> Double         -- ^ initial temperature T0
       -> Double         -- ^ cooling factor alpha (0 < alpha < 1)
       -> Int            -- ^ total number of iterations
       -> StdGen
       -> (Assignment, Int)   -- ^ best assignment and its energy
anneal g k t0 alpha steps gen0 = go 0 t0 gen0 initAssign initE initAssign initE
  where
    (initAssign, gen1) = randomAssignment k (numVertices g) gen0
    initE = energy g initAssign

    go :: Int -> Double -> StdGen -> Assignment -> Int -> Assignment -> Int -> (Assignment, Int)
    go i temp gen curAssign curE bestAssign bestE
        | i >= steps = (bestAssign, bestE)
        | otherwise  =
            let (candAssign, gen1) = neighbor k g curAssign gen
                candE = energy g candAssign
                delta = candE - curE
                (accept, gen2) = if delta <= 0
                                 then (True, gen1)
                                 else let (r, g') = randomR (0.0, 1.0) gen1
                                      in (r < exp (fromIntegral (-delta) / temp), g')
                (nextAssign, nextE) = if accept then (candAssign, candE) else (curAssign, curE)
                (nextBest, nextBestE) = if nextE < bestE then (nextAssign, nextE) else (bestAssign, bestE)
                nextTemp = temp * alpha
            in go (i+1) nextTemp gen2 nextAssign nextE nextBest nextBestE
