module Engine
  ( initialColoring
  , neighborColoring
  , conflicts
  , simulatedAnnealing
  , acceptanceProbability
  , coolingSchedule
  ) where

import Types
import System.Random (randomR, mkStdGen, split)

-- | Generate an initial random coloring.
initialColoring :: Graph -> Int -> RNG -> (Coloring, RNG)
initialColoring graph maxColor gen =
  let (gen1, gen2) = split gen
      colors = map (\g -> fst $ randomR (0, maxColor - 1) g) (replicate (length graph) gen1)
  in (colors, gen2)

-- | Generate a neighboring coloring by recoloring a random vertex.
neighborColoring :: Graph -> Int -> Coloring -> RNG -> (Coloring, RNG)
neighborColoring graph maxColor coloring gen =
  let n = length graph
      (vertexIdx, gen1) = randomR (0, n - 1) gen
      (newColor, gen2) = randomR (0, maxColor - 1) gen1
      newColoring = take vertexIdx coloring ++ [newColor] ++ drop (vertexIdx + 1) coloring
  in (newColoring, gen2)

-- | Count the number of conflicting edges (both endpoints have the same color).
conflicts :: Graph -> Coloring -> Int
conflicts graph coloring =
  let n = length graph
      edgeConflicts i =
        let colorI = coloring !! i
            neighbors = graph !! i
            conflictsWith j = if j > i && coloring !! j == colorI then 1 else 0
        in sum (map conflictsWith neighbors)
  in sum (map edgeConflicts [0 .. n - 1])

-- | Acceptance probability for a worse solution.
acceptanceProbability :: Double -> Int -> Double -> Double
acceptanceProbability temp delta tempFactor =
  exp (-(fromIntegral delta) / temp)

-- | Cooling schedule: exponential decay.
coolingSchedule :: Double -> Double -> Double
coolingSchedule temp coolingRate = temp * coolingRate

-- | Simulated annealing algorithm.
simulatedAnnealing :: Graph -> SAParams -> Int -> RNG -> (Coloring, Int, RNG)
simulatedAnnealing graph (initTemp, coolingRate, minTemp, maxIter) maxColor gen =
  let (currentColoring, gen1) = initialColoring graph maxColor gen
      currentConflicts = conflicts graph currentColoring
      loop temp iter bestColoring bestConflicts currentColoring currentConflicts rng
        | temp < minTemp || iter >= maxIter = (bestColoring, bestConflicts, rng)
        | otherwise =
            let (neighbor, rng1) = neighborColoring graph maxColor currentColoring rng
                neighborConflicts = conflicts graph neighbor
                delta = neighborConflicts - currentConflicts
                accept = if delta <= 0
                         then True
                         else (acceptanceProbability temp delta coolingRate) > (fst $ randomR (0.0, 1.0) rng1)
                (newBestColoring, newBestConflicts) =
                  if neighborConflicts < bestConflicts
                  then (neighbor, neighborConflicts)
                  else (bestColoring, bestConflicts)
                (newCurrentColoring, newCurrentConflicts) =
                  if accept
                  then (neighbor, neighborConflicts)
                  else (currentColoring, currentConflicts)
                newTemp = coolingSchedule temp coolingRate
            in loop newTemp (iter + 1) newBestColoring newBestConflicts newCurrentColoring newCurrentConflicts rng1
  in loop initTemp 0 currentColoring currentConflicts currentColoring currentConflicts gen1
