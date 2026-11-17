module Core (
    Graph,
    SAParams(..),
    simulatedAnnealing,
    bruteForceOptimal,
    ) where

import System.Random (RandomGen, randomR)
import Data.Array (Array, bounds, (!), listArray)
import Data.List (minimumBy, delete, foldl')
import Data.Ord (comparing)

-- | Undirected complete graph represented by a symmetric distance matrix.
type Graph = Array (Int, Int) Double

-- | Parameters controlling the annealing schedule.
data SAParams = SAParams
    { initialTemp :: Double   -- ^ Starting temperature
    , finalTemp   :: Double   -- ^ Temperature at which the algorithm stops
    , alpha       :: Double   -- ^ Multiplicative cooling factor (0 < alpha < 1)
    , iterPerTemp :: Int      -- ^ Number of neighbour trials per temperature
    } deriving (Show, Eq)

-- | Compute the total tour length of a permutation of nodes.
tourCost :: Graph -> [Int] -> Double
tourCost g tour = sum $ zipWith edgeCost tour (tail tour ++ [head tour])
  where
    edgeCost a b = g ! ordered (a, b)
    ordered (x, y) = if x <= y then (x, y) else (y, x)

-- | Generate a neighbour by swapping two distinct positions.
swapNeighbour :: RandomGen g => [Int] -> g -> ([Int], g)
swapNeighbour xs gen =
    let (i, gen')  = randomR (0, n-1) gen
        (j, gen'') = randomR (0, n-2) gen'
        j' = if j >= i then j+1 else j
        swap a b = let vi = xs !! a
                       vj = xs !! b
                   in [ if k == a then vj else if k == b then vi else xs !! k | k <- [0..n-1] ]
    in (swap i j', gen'')
  where
    n = length xs

-- | Acceptance probability for a worse solution.
acceptProb :: Double -> Double -> Double -> Double
acceptProb oldCost newCost temp
    | newCost < oldCost = 1.0
    | otherwise         = exp (-(newCost - oldCost) / temp)

-- | Core simulated annealing loop (pure).
annealLoop
    :: RandomGen g
    => Graph          -- ^ Problem instance
    -> SAParams       -- ^ Annealing parameters
    -> [Int]          -- ^ Current solution
    -> Double         -- ^ Current cost
    -> Double         -- ^ Current temperature
    -> g              -- ^ Random generator
    -> ([Int], Double) -- ^ Best solution found and its cost
annealLoop _ _ bestSol bestCost _ _ = (bestSol, bestCost)  -- placeholder for termination (never reached)

-- | Helper that performs the annealing process.
anneal
    :: RandomGen g
    => Graph -> SAParams -> g -> ([Int], Double)
anneal g params gen0 = go initSol initCost initTemp initSol initCost gen0
  where
    (low, high) = bounds g
    nodes = [i | i <- [fst low .. fst high]]
    initSol = nodes
    initCost = tourCost g initSol
    initTemp = initialTemp params

    go curSol curCost temp bestSol bestCost gen
        | temp <= finalTemp params = (bestSol, bestCost)
        | otherwise = 
            let (curSol', curCost', bestSol', bestCost', gen') =
                    iterateStep curSol curCost bestSol bestCost gen (iterPerTemp params)
                temp' = temp * alpha params
            in go curSol' curCost' temp' bestSol' bestCost' gen'

    iterateStep cs cc bs bc g 0 = (cs, cc, bs, bc, g)
    iterateStep cs cc bs bc g k =
        let (cand, g1) = swapNeighbour cs g
            candCost = tourCost g cand
            prob = acceptProb cc candCost temp
            (randVal, g2) = randomR (0.0, 1.0) g1
            (cs', cc') = if randVal < prob then (cand, candCost) else (cs, cc)
            (bs', bc') = if cc' < bc then (cs', cc') else (bs, bc)
        in iterateStep cs' cc' bs' bc' g2 (k-1)

-- | Public interface.
simulatedAnnealing :: RandomGen g => Graph -> SAParams -> g -> ([Int], Double)
simulatedAnnealing g params gen = anneal g params gen

-- | Brute‑force optimal tour for verification on tiny instances.
bruteForceOptimal :: Graph -> ([Int], Double)
bruteForceOptimal g = minimumBy (comparing snd) tours
  where
    (low, high) = bounds g
    nodes = [i | i <- [fst low .. fst high]]
    perms = permutations nodes
    tours = [(p, tourCost g p) | p <- perms]

-- | Simple permutation generator (no external libs).
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = [ y:ys | y <- xs, ys <- permutations (delete y xs) ]
