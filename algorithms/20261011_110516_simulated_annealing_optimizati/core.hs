module Core (
    Graph,
    makeGraph,
    initialTour,
    tourEnergy,
    simulatedAnnealing
) where

import System.Random (RandomGen, randomR)
import Data.Array (Array, listArray, (!), bounds)
import Data.List (foldl')
import Control.Monad (replicateM)

-- | Undirected weighted complete graph represented as an array indexed by (i,j)
type Graph = Array (Int, Int) Int

-- | Construct a graph from number of nodes and a list of edges (i, j, weight).
--   The graph is assumed symmetric; missing edges are filled with 0.
makeGraph :: Int -> [(Int, Int, Int)] -> Graph
makeGraph n edges = listArray ((0,0),(n-1,n-1)) [weight i j | i <- [0..n-1], j <- [0..n-1]]
  where
    edgeMap = [ ((i,j), w) | (i,j,w) <- edges ] ++ [ ((j,i), w) | (i,j,w) <- edges ]
    weight i j
        | i == j    = 0
        | otherwise = case lookup (i,j) edgeMap of
                        Just w  -> w
                        Nothing -> error $ "Missing edge weight for (" ++ show i ++ "," ++ show j ++ ")"

-- | Compute total length of a tour (cycle) visiting all nodes and returning to start.
tourEnergy :: Graph -> [Int] -> Int
tourEnergy g tour = sum $ zipWith edgeCost tour (tail tour ++ [head tour])
  where
    edgeCost a b = g ! (a,b)

-- | Generate an initial random tour with node 0 fixed at the start.
initialTour :: RandomGen g => Int -> g -> ([Int], g)
initialTour n gen = let (shuffled, gen') = shuffle [1..n-1] gen
                    in (0 : shuffled, gen')

-- Fisher–Yates shuffle (pure)
shuffle :: RandomGen g => [a] -> g -> ([a], g)
shuffle xs gen = go (length xs) xs gen
  where
    go 0 xs' g = (xs', g)
    go i xs' g =
        let (j, g') = randomR (0, i-1) g
            xs'' = swap (i-1) j xs'
        in go (i-1) xs'' g'
    swap i j lst
        | i == j    = lst
        | otherwise = let vi = lst !! i
                          vj = lst !! j
                      in replace i vj (replace j vi lst)
    replace idx val lst = take idx lst ++ [val] ++ drop (idx+1) lst

-- | Produce a neighbor tour by swapping two non‑first positions.
neighborTour :: RandomGen g => [Int] -> g -> ([Int], g)
neighborTour tour gen =
    let n = length tour
        (i, gen1) = randomR (1, n-1) gen
        (j, gen2) = randomR (1, n-1) gen1
        swapped = swap i j tour
    in (swapped, gen2)
  where
    swap a b lst
        | a == b    = lst
        | otherwise = let va = lst !! a
                          vb = lst !! b
                      in replace a vb (replace b va lst)
    replace idx val lst = take idx lst ++ [val] ++ drop (idx+1) lst

-- | Simulated annealing driver.
--   startTemp: initial temperature,
--   cooling: multiplicative cooling factor (0 < cooling < 1),
--   steps: total number of iterations.
simulatedAnnealing :: RandomGen g
                   => Graph          -- ^ problem graph
                   -> Double         -- ^ initial temperature
                   -> Double         -- ^ cooling factor
                   -> Int            -- ^ number of steps
                   -> g              -- ^ random generator
                   -> ([Int], g)     -- ^ final tour and generator
simulatedAnnealing g startTemp cooling steps gen0 =
    let n = let ((0,0),(maxIdx,_)) = bounds g in maxIdx + 1
        (initTour, gen1) = initialTour n gen0
        initEnergy = fromIntegral $ tourEnergy g initTour
    in annealLoop initTour initEnergy startTemp steps gen1
  where
    annealLoop tour energy temp 0 gen = (tour, gen)
    annealLoop tour energy temp k gen =
        let (candTour, gen1) = neighborTour tour gen
            candEnergy = fromIntegral $ tourEnergy g candTour
            delta = candEnergy - energy
            acceptProb = if delta <= 0
                         then 1.0
                         else exp ( - delta / temp )
            (r, gen2) = randomR (0.0, 1.0) gen1
            (newTour, newEnergy) = if r < acceptProb
                                   then (candTour, candEnergy)
                                   else (tour, energy)
            newTemp = temp * cooling
        in annealLoop newTour newEnergy newTemp (k-1) gen2
