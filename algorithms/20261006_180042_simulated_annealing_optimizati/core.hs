module Core where

import System.Random
import Data.List
import Data.Function (on)

-- | A solution is represented as a list of Bool indicating inclusion of each element.
type Solution = [Bool]

-- | Compute the cost of a solution given a list of weights and a target sum.
-- Lower cost is better. Cost is the absolute difference between the subset sum and the target.
cost :: [Int] -> Int -> Solution -> Double
cost weights target sol =
    let subsetSum = sum [w | (w, True) <- zip weights sol]
    in fromIntegral (abs (subsetSum - target))

-- | Generate a neighboring solution by flipping a random bit.
neighbor :: RandomGen g => Solution -> g -> (Solution, g)
neighbor sol gen =
    let (idx, gen') = randomR (0, length sol - 1) gen
        newSol = take idx sol ++ [not (sol !! idx)] ++ drop (idx + 1) sol
    in (newSol, gen')

-- | Simulated annealing algorithm.
-- Parameters:
--   weights      : list of element weights
--   target       : desired target sum
--   initSol      : initial solution
--   initTemp     : initial temperature
--   coolingRate  : factor < 1 to reduce temperature each iteration
--   maxIter      : maximum number of iterations
--   gen          : random number generator
simulatedAnnealing :: RandomGen g
                   => [Int] -> Int -> Solution -> Double -> Double -> Int -> g
                   -> Solution
simulatedAnnealing weights target initSol initTemp coolingRate maxIter gen =
    let
        go :: Int -> Double -> Solution -> Double -> Solution -> g -> Solution
        go 0 _ _ _ best _ = best
        go iter temp curSol curCost best gen' =
            let (nextSol, gen'') = neighbor curSol gen'
                nextCost = cost weights target nextSol
                delta = nextCost - curCost
                accept = if delta <= 0
                         then True
                         else let (r, gen''') = randomR (0.0, 1.0) gen''
                                  prob = exp (-delta / temp)
                              in r < prob
                (newSol, newCost) = if accept then (nextSol, nextCost) else (curSol, curCost)
                newBest = if newCost < curCost && newCost < cost weights target best
                          then newSol
                          else best
                newTemp = temp * coolingRate
            in go (iter - 1) newTemp newSol newCost newBest gen'''
    in go maxIter initTemp initSol (cost weights target initSol) initSol gen

-- | Helper to create an initial random solution.
randomSolution :: RandomGen g => Int -> g -> (Solution, g)
randomSolution n gen =
    let (bools, gen') = foldl (\(bs, g) _ ->
                                let (b, g') = random g
                                in (b:bs, g')) ([], gen) [1..n]
    in (reverse bools, gen')
