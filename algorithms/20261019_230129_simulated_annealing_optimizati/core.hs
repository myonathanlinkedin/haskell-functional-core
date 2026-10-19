module Core (
    Vertex,
    Point,
    Graph,
    SAParams(..),
    anneal,
    totalDistance,
    initialSolution
) where

import System.Random (RandomGen, split, mkStdGen, StdGen)
import Data.Array (Array, listArray, bounds, (!), indices)
import Data.List (foldl')
import Control.Monad (when)

type Vertex = Int
type Point  = (Double, Double)
type Graph  = Array Vertex Point

data SAParams = SAParams
    { initialTemp       :: Double   -- ^ Starting temperature
    , coolingRate       :: Double   -- ^ Multiplicative cooling factor (0 < rate < 1)
    , iterationsPerTemp :: Int      -- ^ Number of neighbor trials per temperature
    , minTemp           :: Double   -- ^ Temperature at which to stop
    } deriving (Show, Eq)

-- Euclidean distance between two points
euclidean :: Point -> Point -> Double
euclidean (x1, y1) (x2, y2) = sqrt ((x1 - x2) ^ (2 :: Int) + (y1 - y2) ^ (2 :: Int))

-- Total length of a tour (closed loop)
totalDistance :: Graph -> [Vertex] -> Double
totalDistance g tour = case tour of
    []  -> 0
    [_] -> 0
    _   -> let pairs = zip tour (tail tour ++ [head tour])
           in foldl' (\acc (a,b) -> acc + euclidean (g ! a) (g ! b)) 0 pairs

-- Produce the initial solution: vertices in natural order
initialSolution :: Graph -> [Vertex]
initialSolution = indices

-- Swap two elements at given indices in a list (pure)
swap :: Int -> Int -> [a] -> [a]
swap i j xs
    | i == j    = xs
    | otherwise = let vi = xs !! i
                      vj = xs !! j
                      replace k v ys = take k ys ++ [v] ++ drop (k+1) ys
                  in replace i vj (replace j vi xs)

-- Generate a neighbor by swapping two distinct random positions
randomSwap :: RandomGen g => [a] -> g -> ([a], g)
randomSwap xs gen
    | n < 2    = (xs, gen)
    | otherwise = (swap i j xs, gen'')
  where
    n = length xs
    (i, gen')  = randomR (0, n-1) gen
    (jTmp, gen'') = randomR (0, n-2) gen'
    j = if jTmp >= i then jTmp + 1 else jTmp

-- Acceptance probability for a worse solution
acceptProb :: Double -> Double -> Double
acceptProb delta temp = exp ( - delta / temp )

-- Perform a single temperature phase
annealPhase :: RandomGen g
            => Graph
            -> SAParams
            -> Double               -- ^ current temperature
            -> [Vertex]             -- ^ current solution
            -> Double               -- ^ current cost
            -> g
            -> ([Vertex], Double, g) -- ^ resulting solution, cost, generator
annealPhase g params temp sol cost gen = go sol cost gen (iterationsPerTemp params)
  where
    go s c rng 0 = (s, c, rng)
    go s c rng k =
        let (rng1, rng2) = split rng
            (candidate, rng3) = randomSwap s rng1
            candCost = totalDistance g candidate
            delta = candCost - c
        in if delta <= 0
           then go candidate candCost rng3 (k-1)
           else let (r, rng4) = randomR (0.0, 1.0) rng3
                in if r < acceptProb delta temp
                   then go candidate candCost rng4 (k-1)
                   else go s c rng4 (k-1)

-- Main simulated annealing driver (pure)
anneal :: RandomGen g => Graph -> SAParams -> g -> ([Vertex], Double, g)
anneal g params gen0 = go initSol initCost (initialTemp params) gen0
  where
    initSol  = initialSolution g
    initCost = totalDistance g initSol
    go sol cost temp gen
        | temp < minTemp params = (sol, cost, gen)
        | otherwise =
            let (newSol, newCost, gen') = annealPhase g params temp sol cost gen
                nextTemp = temp * coolingRate params
            in go newSol newCost nextTemp gen'
