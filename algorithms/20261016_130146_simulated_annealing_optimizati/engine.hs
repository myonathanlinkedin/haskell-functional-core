module Engine
  ( cost
  , neighbor
  , accept
  , runSA
  ) where

import Types
import System.Random (StdGen, randomR)

-- | Total length of the tour (assumes a round‑trip back to the start vertex).
cost :: Graph -> Tour -> Double
cost g (Tour vs) = sum $ zipWith edgeCost vs (tail vs ++ [head vs])
  where
    edgeCost a b = (g !! a) !! b

-- | Produce a neighbouring tour by swapping two randomly chosen vertices.
neighbor :: StdGen -> Tour -> (Tour, StdGen)
neighbor gen (Tour vs) = (Tour newVs, gen'')
  where
    n = length vs
    (i, gen')  = randomR (0, n - 1) gen
    (j, gen'') = randomR (0, n - 1) gen'
    swap xs a b = [ if k == a then xs !! b
                    else if k == b then xs !! a
                    else x
                  | (x, k) <- zip xs [0..] ]
    newVs = swap vs i j

-- | Metropolis acceptance test.
--   Returns True if the new state should be accepted.
accept :: Double   -- ^ Cost of current state
       -> Double   -- ^ Cost of candidate state
       -> Double   -- ^ Current temperature
       -> Double   -- ^ Random number in [0,1]
       -> Bool
accept oldCost newCost temp r
  | newCost < oldCost = True
  | otherwise         = exp ((oldCost - newCost) / temp) > r

-- | Core simulated‑annealing loop (pure, threads the random generator).
annealStep :: Graph -> SAParams -> StdGen -> Tour -> Double -> Double
           -> (Tour, Double, StdGen)
annealStep g params gen curTour curCost temp
  | temp <= finalTemp params = (curTour, curCost, gen)
  | otherwise                = iterateTemp gen curTour curCost temp (iterPerTemp params)
  where
    iterateTemp g' tour cost t 0 = annealStep g params g' tour cost (t * alpha params)
    iterateTemp g' tour cost t k =
      let (candTour, g1)   = neighbor g' tour
          candCost          = cost g candTour
          (r, g2)           = randomR (0.0, 1.0) g1
          keep              = accept cost candCost t r
          (nextTour, nextCost) = if keep then (candTour, candCost) else (tour, cost)
      in iterateTemp g2 nextTour nextCost t (k - 1)

-- | Public entry point: runs simulated annealing and returns the best tour found.
runSA :: Graph -> SAParams -> StdGen -> Tour -> (Tour, Double)
runSA g params gen initTour = (bestTour, bestCost)
  where
    initCost = cost g initTour
    (bestTour, bestCost, _) = annealStep g params gen initTour initCost (initialTemp params)
