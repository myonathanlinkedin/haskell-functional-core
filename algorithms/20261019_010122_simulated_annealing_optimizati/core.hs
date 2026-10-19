module Core (
    Graph,
    initialSolution,
    cost,
    neighbor,
    acceptanceProbability,
    anneal,
    defaultCooling
) where

import System.Random (StdGen, randomR)
import Data.List (foldl')
import Control.Monad (guard)

-- | A complete undirected weighted graph represented by a distance matrix.
--   graph !! i !! j is the distance from vertex i to vertex j.
type Graph = [[Double]]

-- | Produce an initial solution: the identity permutation of vertices.
initialSolution :: Int -> [Int]
initialSolution n = [0 .. n - 1]

-- | Compute the total length of the tour defined by a permutation of vertices.
--   The tour is assumed to be a cycle returning to the start vertex.
cost :: Graph -> [Int] -> Double
cost g tour = foldl' (+) 0 edges
  where
    n = length tour
    idx i = tour !! i
    edges = [ g !! idx i !! idx ((i + 1) `mod` n) | i <- [0 .. n - 1] ]

-- | Generate a neighboring solution by swapping two distinct positions.
--   Returns the new permutation and the updated random generator.
neighbor :: [Int] -> StdGen -> ([Int], StdGen)
neighbor xs gen0 = (swap i j xs, gen2)
  where
    n = length xs
    (i, gen1) = randomR (0, n - 1) gen0
    (j, gen2) = randomR (0, n - 1) gen1
    swap a b lst
        | a == b    = lst
        | otherwise = let vi = lst !! a
                          vj = lst !! b
                          replace k v l = take k l ++ [v] ++ drop (k + 1) l
                      in replace a vj (replace b vi lst)

-- | Compute the acceptance probability for a worse solution.
--   If the new cost is lower, probability is 1.0.
acceptanceProbability :: Double -> Double -> Double -> Double
acceptanceProbability oldCost newCost temperature
    | newCost < oldCost = 1.0
    | otherwise = exp (-(newCost - oldCost) / temperature)

-- | Default exponential cooling schedule.
defaultCooling :: Double -> Double -> Double
defaultCooling alpha t = alpha * t

-- | Simulated annealing optimizer.
--   Parameters:
--     graph          – problem instance
--     initSol        – starting permutation
--     initTemp       – initial temperature (>0)
--     alpha          – cooling factor (0 < alpha < 1)
--     iterPerTemp    – number of neighbor trials per temperature level
--     gen            – random generator seed
--   Returns the best solution found and its cost.
anneal :: Graph -> [Int] -> Double -> Double -> Int -> StdGen -> ([Int], Double)
anneal graph initSol initTemp alpha iterPerTemp gen0 = go initTemp initSol initCost initSol initCost gen0
  where
    initCost = cost graph initSol

    go :: Double -> [Int] -> Double -> [Int] -> Double -> StdGen -> ([Int], Double)
    go temp curSol curCost bestSol bestCost gen
        | temp <= 1e-8 = (bestSol, bestCost)  -- termination condition
        | otherwise    = let (curSol', curCost', bestSol', bestCost', gen') =
                              iterateStep iterPerTemp curSol curCost bestSol bestCost gen
                          in go (defaultCooling alpha temp) curSol' curCost' bestSol' bestCost' gen'

    iterateStep :: Int -> [Int] -> Double -> [Int] -> Double -> StdGen -> ([Int], Double, [Int], Double, StdGen)
    iterateStep 0 cs cc bs bc g = (cs, cc, bs, bc, g)
    iterateStep k cs cc bs bc g =
        let (candSol, g1) = neighbor cs g
            candCost = cost graph candSol
            prob = acceptanceProbability cc candCost temp
            (r, g2) = randomR (0.0, 1.0) g1
            (newCs, newCc) = if r < prob then (candSol, candCost) else (cs, cc)
            (newBs, newBc) = if candCost < bc then (candSol, candCost) else (bs, bc)
        in iterateStep (k - 1) newCs newCc newBs newBc g2
