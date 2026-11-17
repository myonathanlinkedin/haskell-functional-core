module Main where

import Core
import System.Random (mkStdGen)
import Control.Exception (assert)
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

-- | Small symmetric graph (5 nodes) with known optimal TSP tour cost = 10.0
sampleGraph :: Graph
sampleGraph = listArray ((1,1),(5,5))
    [ 0, 2, 9, 10, 7
    , 2, 0, 6, 4, 3
    , 9, 6, 0, 8, 5
    ,10, 4, 8, 0, 6
    , 7, 3, 5, 6, 0
    ]

-- | Parameters tuned for the tiny instance.
sampleParams :: SAParams
sampleParams = SAParams
    { initialTemp = 100.0
    , finalTemp   = 0.1
    , alpha       = 0.95
    , iterPerTemp = 200
    }

-- | Verify that simulated annealing finds the optimal cost for the sample graph.
testSimulatedAnnealing :: IO ()
testSimulatedAnnealing = do
    let seed = 42
        gen = mkStdGen seed
        (saTour, saCost) = simulatedAnnealing sampleGraph sampleParams gen
        (optTour, optCost) = bruteForceOptimal sampleGraph
        tolerance = 1e-9
        ok = abs (saCost - optCost) <= tolerance
    putStrLn $ "Optimal cost (brute force): " ++ show optCost
    putStrLn $ "SA cost: " ++ show saCost
    putStrLn $ "SA tour: " ++ show saTour
    putStrLn $ "Optimal tour (one of): " ++ show optTour
    if ok
        then putStrLn "Test passed."
        else do
            putStrLn "Test failed: SA did not reach optimal cost."
            exitFailure

main :: IO ()
main = do
    testSimulatedAnnealing
    exitSuccess
