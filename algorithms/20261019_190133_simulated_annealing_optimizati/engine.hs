module Engine ( anneal
              , tourCost
              , initialTour
              ) where

import Types
import System.Random (randomR, split)

-- | Compute the total length of a tour on the given graph.
tourCost :: Graph -> Tour -> Double
tourCost g tour = sum $ zipWith edgeCost cyclic tour
  where
    n = length tour
    cyclic = take n $ drop 1 $ cycle tour
    edgeCost a b = (g !! a) !! b

-- | Produce an initial tour (identity permutation) for a graph with n vertices.
initialTour :: Int -> Tour
initialTour n = [0 .. n-1]

-- | Generate a neighbouring tour by swapping two distinct positions.
--   Returns the new tour together with the updated RNG.
neighbor :: RNG -> Tour -> (Tour, RNG)
neighbor rng tour = (newTour, rng'')
  where
    (i, rng')  = randomR (0, n-1) rng
    (j, rng'') = randomR (0, n-1) rng'
    n = length tour
    -- Ensure i /= j; if equal, swap with next index modulo n
    (i', j') = if i == j then (i, (j+1) `mod` n) else (i, j)
    newTour = swap i' j' tour

    swap :: Int -> Int -> [a] -> [a]
    swap x y xs = let
        elemX = xs !! x
        elemY = xs !! y
        replace idx val lst = take idx lst ++ [val] ++ drop (idx+1) lst
      in replace x elemY (replace y elemX xs)

-- | Acceptance probability for a worse solution.
acceptProb :: Double -> Double -> Double -> Double
acceptProb oldCost newCost temp
  | newCost < oldCost = 1.0
  | otherwise         = exp (-(newCost - oldCost) / temp)

-- | Single iteration at a given temperature: try a fixed number of neighbours.
stepTemp :: Graph -> SAParams -> Double -> Tour -> Double -> RNG -> (Tour, Double, RNG)
stepTemp g params temp curTour curCost rng = go (iterPerTemp params) curTour curCost rng
  where
    go 0 bestTour bestCost r = (bestTour, bestCost, r)
    go k bestTour bestCost r =
      let (candTour, r1) = neighbor r bestTour
          candCost = tourCost g candTour
          prob = acceptProb bestCost candCost temp
          (u, r2) = randomR (0.0, 1.0) r1
          (nextTour, nextCost) = if u < prob
                                 then (candTour, candCost)
                                 else (bestTour, bestCost)
      in go (k-1) nextTour nextCost r2

-- | Main annealing loop. Purely functional; returns the best tour found.
anneal :: Graph -> SAParams -> RNG -> SAResult
anneal g params rng0 = go (initialTemp params) rng0 initTour initCost
  where
    n = length g
    initTour = initialTour n
    initCost = tourCost g initTour

    go temp rng curTour curCost
      | temp <= finalTemp params = SAResult curTour curCost temp
      | otherwise =
          let (newTour, newCost, rng') = stepTemp g params temp curTour curCost rng
              nextTemp = temp * alpha params
          in go nextTemp rng' newTour newCost
