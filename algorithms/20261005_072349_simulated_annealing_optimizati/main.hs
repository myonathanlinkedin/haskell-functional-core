module Main where

import System.Random (StdGen, mkStdGen, randomR)
import Data.List (minimumBy, foldl')
import Data.Function (on)
import Control.Monad (when)
import System.Exit (exitFailure)

type Point = (Double, Double)
type Tour  = [Int]

-- Euclidean distance between two points
dist :: Point -> Point -> Double
dist (x1,y1) (x2,y2) = sqrt ((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))

-- Total length of a tour (assumes return to start)
tourLength :: [Point] -> Tour -> Double
tourLength pts tour = sum $ zipWith d (cycle tour) (tail $ cycle tour)
  where
    d i j = dist (pts !! i) (pts !! j)

-- Generate a neighboring tour by swapping two random positions
neighbor :: RandomGen g => Tour -> g -> (Tour, g)
neighbor t g =
    let (i, g1) = randomR (0, n-1) g
        (j, g2) = randomR (0, n-1) g1
        n = length t
        swap xs a b = let xa = xs !! a
                          xb = xs !! b
                      in [ if k == a then xb else if k == b then xa else xs !! k | k <- [0..n-1] ]
    in (swap t i j, g2)

-- Simulated annealing core
simulatedAnnealing
    :: RandomGen g
    => Double        -- ^ initial temperature
    -> Double        -- ^ cooling factor (0 < factor < 1)
    -> Int           -- ^ iterations per temperature
    -> Double        -- ^ minimum temperature
    -> ([Point] -> Tour -> Double) -- ^ cost function
    -> ([Point] -> Tour -> g -> (Tour, g)) -- ^ neighbor generator
    -> [Point]       -- ^ problem data
    -> Tour          -- ^ initial solution
    -> g             -- ^ random generator
    -> Tour          -- ^ best solution found
simulatedAnnealing t0 cool iterPerTemp tMin cost neigh pts initTour gen = go t0 initTour initTour (cost pts initTour) gen
  where
    go temp cur best bestCost g
        | temp <= tMin = best
        | otherwise    = let (cur', best', bestCost', g') = innerIter iterPerTemp cur best bestCost g
                         in go (temp * cool) cur' best' bestCost' g'
    innerIter 0 cur best bestCost g = (cur, best, bestCost, g)
    innerIter k cur best bestCost g =
        let (cand, g1) = neigh pts cur g
            candCost   = cost pts cand
            curCost    = cost pts cur
            acceptProb = if candCost < curCost
                         then 1.0
                         else exp (-(candCost - curCost) / temp)
            (r, g2)    = randomR (0.0, 1.0) g1
            cur'       = if r < acceptProb then cand else cur
            (best', bestCost') = if candCost < bestCost then (cand, candCost) else (best, bestCost)
        in innerIter (k-1) cur' best' bestCost' g2

-- Simple deterministic initial tour (identity)
initialTour :: Int -> Tour
initialTour n = [0..n-1]

-- Unit test utilities
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = putStrLn ("ASSERTION FAILED: " ++ msg) >> exitFailure

-- Test case: square with side length 1, optimal tour length = 4.0
testSquare :: IO ()
testSquare = do
    let pts = [(0,0),(1,0),(1,1),(0,1)]
        n   = length pts
        initT = initialTour n
        gen = mkStdGen 42
        best = simulatedAnnealing
                10.0   -- initial temperature
                0.95   -- cooling factor
                200    -- iterations per temperature
                1e-3   -- minimum temperature
                tourLength
                neighbor
                pts
                initT
                gen
        bestLen = tourLength pts best
    assert (abs (bestLen - 4.0) < 0.1) ("Square test failed, length = " ++ show bestLen)

-- Example run with random points
runExample :: IO ()
runExample = do
    let seed = 12345
        gen0 = mkStdGen seed
        (pts, gen1) = generatePoints 20 gen0
        n = length pts
        initT = initialTour n
        best = simulatedAnnealing
                100.0
                0.93
                500
                1e-4
                tourLength
                neighbor
                pts
                initT
                gen1
        bestLen = tourLength pts best
    putStrLn "Best tour found:"
    print best
    putStrLn ("Tour length: " ++ show bestLen)

-- Generate n random points in [0,10]x[0,10]
generatePoints :: RandomGen g => Int -> g -> ([Point], g)
generatePoints 0 g = ([], g)
generatePoints k g =
    let (x, g1) = randomR (0.0, 10.0) g
        (y, g2) = randomR (0.0, 10.0) g1
        (rest, g3) = generatePoints (k-1) g2
    in ((x,y):rest, g3)

main :: IO ()
main = do
    testSquare
    runExample
    putStrLn "All tests passed."
