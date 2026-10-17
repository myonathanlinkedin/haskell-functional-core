module Engine where

import Types
import System.Random (randomR, StdGen)
import Data.Array (Array, (!), bounds)
import Data.List (minimumBy)
import Data.Ord (comparing)

-- | Compute the total length of a tour on the given graph.
cost :: Graph -> Path -> Double
cost _ []  = 0
cost _ [_] = 0
cost g p   = sum [ g ! edge | edge <- zip p (tail p ++ [head p]) ]

-- | Generate a neighbouring solution using a 2‑opt move.
neighbor :: Path -> RNG -> (Path, RNG)
neighbor p gen
  | n < 2    = (p, gen)  -- no possible move
  | otherwise = (newPath, gen'')
  where
    n = length p
    (i, gen')   = randomR (0, n - 2) gen
    (j, gen'')  = randomR (i + 1, n - 1) gen'
    (prefix, rest) = splitAt i p
    (mid, suffix)  = splitAt (j - i + 1) rest
    newPath = prefix ++ reverse mid ++ suffix

-- | Acceptance probability for a worse candidate.
acceptProb :: Double -> Double -> Double
acceptProb delta temp = exp ( - delta / temp )

-- | Core simulated annealing loop.
anneal :: Graph -> SAParams -> RNG -> Path
anneal g params rng0 = go (initialTemp params) initPath rng0 initPath initCost
  where
    ((lo,_),(hi,_)) = bounds g
    n = hi - lo + 1
    initPath = [lo .. hi]               -- deterministic initial tour
    initCost = cost g initPath

    go :: Double -> Path -> RNG -> Path -> Double -> Path
    go temp curPath rng bestPath bestCost
      | temp <= finalTemp params = bestPath
      | otherwise                = go temp' curPath' rng' bestPath' bestCost'
      where
        (curPath', rng', bestPath', bestCost') = innerLoop (innerIter params) temp curPath rng bestPath bestCost
        temp' = temp * alpha params

    innerLoop :: Int -> Double -> Path -> RNG -> Path -> Double -> (Path, RNG, Path, Double)
    innerLoop 0 _ curPath rng bestPath bestCost = (curPath, rng, bestPath, bestCost)
    innerLoop k temp curPath rng bestPath bestCost =
      let (candPath, rng1) = neighbor curPath rng
          curCost = cost g curPath
          candCost = cost g candPath
          delta = candCost - curCost
          (accept, rng2) = if delta < 0
                           then (True, rng1)
                           else
                             let (r, g') = randomR (0.0, 1.0) rng1
                                 prob = acceptProb delta temp
                             in (r < prob, g')
          newCurPath = if accept then candPath else curPath
          newCurCost = if accept then candCost else curCost
          (newBestPath, newBestCost) = if newCurCost < bestCost
                                       then (newCurPath, newCurCost)
                                       else (bestPath, bestCost)
      in innerLoop (k - 1) temp newCurPath rng2 newBestPath newBestCost
