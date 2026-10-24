module Main where

import Types
import Engine
import System.Random (mkStdGen)

-- Simple assertion helper.
assert :: Bool -> String -> IO ()
assert True _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)

-- Example graph (4-node complete graph with symmetric distances).
exampleGraph :: Graph
exampleGraph =
    [ [0, 2, 9,10]
    , [2, 0, 6, 4]
    , [9, 6, 0, 3]
    , [10,4, 3, 0]
    ]

-- Expected optimal tour cost for the example graph is 18.
optimalCost :: Double
optimalCost = 18.0

-- Simulated annealing parameters.
saParams :: SAParams
saParams = SAParams
    { initialTemp = 1000.0
    , coolingRate = 0.95
    , iterationsPerTemp = 200
    , minTemp = 0.1
    }

main :: IO ()
main = do
    let seed = 42
        gen = mkStdGen seed
        finalSol = simulatedAnnealing exampleGraph saParams gen
        finalCost = cost exampleGraph finalSol

    putStrLn $ "Final solution (tour): " ++ show finalSol
    putStrLn $ "Final cost: " ++ show finalCost

    -- Basic sanity checks.
    assert (length finalSol == length exampleGraph) "Solution length mismatch."
    assert (finalCost <= optimalCost + 2.0) "Final cost not within acceptable bound."
    putStrLn "All assertions passed."
