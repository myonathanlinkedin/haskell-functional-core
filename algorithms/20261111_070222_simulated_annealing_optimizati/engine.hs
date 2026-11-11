module Engine (
    anneal,
    acceptanceProbability,
    flipVertex,
    randomVertex
) where

import Types
import qualified Data.IntMap.Strict as IM
import System.Random (StdGen, randomR)

-- | Flip the side of a given vertex in the assignment.
flipVertex :: Assignment -> Vertex -> Assignment
flipVertex assign v = IM.adjust not v assign

-- | Choose a random vertex from the current assignment.
randomVertex :: Assignment -> StdGen -> (Vertex, StdGen)
randomVertex assign gen =
    let (idx, gen') = randomR (0, IM.size assign - 1) gen
        v = fst $ IM.elemAt idx assign
    in (v, gen')

-- | Acceptance probability for simulated annealing.
acceptanceProbability :: Int -> Int -> Double -> Double
acceptanceProbability oldScore newScore temp
    | newScore > oldScore = 1.0
    | otherwise = exp (fromIntegral (newScore - oldScore) / temp)

-- | Perform a single neighbour proposal and possibly accept it.
annealStep :: Graph -> SAParams -> Assignment -> StdGen -> Double -> (Assignment, StdGen)
annealStep g params current gen temp =
    let (v, gen1)      = randomVertex current gen
        neighbor       = flipVertex current v
        curScore       = evaluate g current
        neighScore     = evaluate g neighbor
        prob           = acceptanceProbability curScore neighScore temp
        (r, gen2)      = randomR (0.0, 1.0) gen1
        nextAssign     = if r < prob then neighbor else current
    in (nextAssign, gen2)

-- | Core simulated annealing loop.
anneal :: Graph -> SAParams -> Assignment -> StdGen -> (Assignment, Double)
anneal g params initAssign gen = go initAssign gen (initialTemp params)
  where
    go assign gen' temp
        | temp <= minTemp params = (assign, temp)
        | otherwise =
            let (assign', gen'') = iterateSteps assign gen' (stepsPerTemp params) temp
                temp' = temp * coolingRate params
            in go assign' gen'' temp'

    iterateSteps a g 0 _ = (a, g)
    iterateSteps a g n t =
        let (a', g') = annealStep g params a g t
        in iterateSteps a' g' (n - 1) t
