module Engine
    ( initialSolution
    , cost
    , neighborSolution
    , acceptanceProbability
    , simulatedAnnealing
    ) where

import Types
import System.Random (StdGen, randomR, mkStdGen)

-- | Generate an initial solution: the identity permutation of nodes.
initialSolution :: Graph -> Solution
initialSolution g = [0 .. length g - 1]

-- | Compute the total cost of a tour (including return to the start node).
cost :: Graph -> Solution -> Cost
cost g s = sum $ zipWith edgeCost tour (tail tour ++ [head tour])
  where
    edgeCost a b = (g !! a) !! b
    tour = s

-- | Produce a neighbour solution by swapping two distinct positions.
neighborSolution :: StdGen -> Solution -> (Solution, StdGen)
neighborSolution gen sol = (swap i j sol, gen'')
  where
    n = length sol
    (i, gen')  = randomR (0, n - 1) gen
    (j, gen'') = randomR (0, n - 2) gen'
    j' = if j >= i then j + 1 else j   -- ensure j ≠ i
    swap x y xs = let vi = xs !! x
                      vj = xs !! y
                  in [ if k == x then vj else if k == y then vi else xs !! k | k <- [0..n-1] ]

-- | Acceptance probability for a candidate solution.
acceptanceProbability :: Cost -> Cost -> Double -> Double
acceptanceProbability oldCost newCost temp
    | newCost < oldCost = 1.0
    | otherwise         = exp ((oldCost - newCost) / temp)

-- | Core simulated annealing loop (pure). Returns the best solution found.
simulatedAnnealing :: Graph -> SAParams -> StdGen -> Solution
simulatedAnnealing g params gen0 = go (initialSolution g) (cost g (initialSolution g)) gen0 (initialTemp params)
  where
    go :: Solution -> Cost -> StdGen -> Double -> Solution
    go bestSol bestCost gen temp
        | temp < minTemp params = bestSol
        | otherwise = let (bestSol', bestCost', gen') = iterateTemp bestSol bestCost gen temp (iterationsPerTemp params)
                          temp' = temp * coolingRate params
                      in go bestSol' bestCost' gen' temp'

    -- Perform a fixed number of neighbour evaluations at the current temperature.
    iterateTemp :: Solution -> Cost -> StdGen -> Double -> Int -> (Solution, Cost, StdGen)
    iterateTemp curSol curCost gen _ 0 = (curSol, curCost, gen)
    iterateTemp curSol curCost gen t k =
        let (candSol, gen1) = neighborSolution gen curSol
            candCost = cost g candSol
            prob = acceptanceProbability curCost candCost t
            (r, gen2) = randomR (0.0, 1.0) gen1
            (nextSol, nextCost) = if r < prob
                                  then (candSol, candCost)
                                  else (curSol, curCost)
            (bestSol, bestCost) = if candCost < curCost then (candSol, candCost) else (curSol, curCost)
        in iterateTemp bestSol bestCost gen2 t (k - 1)
