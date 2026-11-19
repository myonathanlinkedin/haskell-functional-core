module Engine (
    cutWeight,
    neighbor,
    acceptProb,
    anneal,
    randomSolution
) where

import Types
import System.Random (StdGen, randomR, random)
import Data.List (foldl')
import Control.Monad (replicateM)

-- | Compute total weight of edges crossing the cut defined by a solution.
cutWeight :: Graph -> Solution -> Weight
cutWeight (Graph _ es) (Solution as) =
    foldl' (\acc (u,v,w) ->
                if (as !! u) /= (as !! v) then acc + w else acc) 0 es

-- | Generate a neighbouring solution by flipping a random vertex.
neighbor :: Solution -> StdGen -> (Solution, StdGen)
neighbor (Solution as) gen =
    let (idx, gen') = randomR (0, length as - 1) gen
        newAs = take idx as ++ [not (as !! idx)] ++ drop (idx + 1) as
    in (Solution newAs, gen')

-- | Acceptance probability for simulated annealing.
acceptProb :: Weight -> Weight -> Double -> Double
acceptProb oldScore newScore temp
    | newScore > oldScore = 1.0
    | otherwise           = exp ((newScore - oldScore) / temp)

-- | Core simulated annealing loop.
anneal :: Graph          -- ^ Problem instance
       -> Solution       -- ^ Initial solution
       -> Double         -- ^ Initial temperature
       -> Double         -- ^ Cooling factor (0 < factor < 1)
       -> Int            -- ^ Iterations per temperature level
       -> StdGen         -- ^ Random generator
       -> (Solution, Weight, StdGen)   -- ^ Best solution, its score, final generator
anneal graph initSol initTemp coolingRate iterPerTemp gen0 = go initSol initScore initTemp gen0 initSol initScore
  where
    initScore = cutWeight graph initSol

    go :: Solution -> Weight -> Double -> StdGen -> Solution -> Weight
       -> (Solution, Weight, StdGen)
    go currentSol currentScore temp gen bestSol bestScore
        | temp <= 1e-8 = (bestSol, bestScore, gen)
        | otherwise    =
            let (gen', (newSol, newScore)) = runIter currentSol currentScore temp iterPerTemp gen (bestSol, bestScore)
                (newBestSol, newBestScore) = if newScore > bestScore then (newSol, newScore) else (bestSol, bestScore)
            in go newSol newScore (temp * coolingRate) gen' newBestSol newBestScore

    -- Perform a fixed number of Metropolis steps at a given temperature.
    runIter :: Solution -> Weight -> Double -> Int -> StdGen -> (Solution, Weight)
            -> (StdGen, (Solution, Weight))
    runIter sol score _ 0 gen acc = (gen, acc)
    runIter sol score temp n gen (bestIterSol, bestIterScore) =
        let (candSol, gen1)   = neighbor sol gen
            candScore         = cutWeight graph candSol
            prob              = acceptProb score candScore temp
            (r, gen2)         = randomR (0.0, 1.0) gen1
            (nextSol, nextScore) = if r < prob then (candSol, candScore) else (sol, score)
            (newBestSol, newBestScore) = if candScore > bestIterScore
                                         then (candSol, candScore)
                                         else (bestIterSol, bestIterScore)
        in runIter nextSol nextScore temp (n - 1) gen2 (newBestSol, newBestScore)

-- | Produce a random initial solution for a given number of vertices.
randomSolution :: Int -> StdGen -> (Solution, StdGen)
randomSolution n gen = (Solution bools, gen')
  where
    (bools, gen') = generate n gen
    generate 0 g = ([], g)
    generate k g =
        let (b, g1)   = random g
            (rest, g2) = generate (k - 1) g1
        in (b : rest, g2)
