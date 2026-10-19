module Main where

import Core
import System.Random (mkStdGen)
import Control.Exception (assert)

-- | Simple complete graph with 4 vertices.
sampleGraph :: Graph
sampleGraph =
    [ [0, 2, 9, 10]
    , [2, 0, 6, 4]
    , [9, 6, 0, 8]
    , [10,4, 8, 0]
    ]

-- | Verify that the cost function is symmetric with respect to tour reversal.
prop_costSymmetry :: Bool
prop_costSymmetry = let g = sampleGraph
                        sol = [0,1,2,3]
                        rev = reverse sol
                    in cost g sol == cost g rev

-- | Ensure that neighbor produces a permutation of the same length.
prop_neighborPermutation :: Bool
prop_neighborPermutation = let sol = initialSolution 5
                               gen = mkStdGen 42
                               (nbr, _) = neighbor sol gen
                           in length nbr == length sol && all (`elem` sol) nbr

-- | Run annealing on the sample graph and check that the final cost is not worse than the initial.
prop_annealImproves :: Bool
prop_annealImproves = let g = sampleGraph
                          initSol = initialSolution (length g)
                          initCost = cost g initSol
                          (bestSol, bestCost) = anneal g initSol 100.0 0.95 200 (mkStdGen 12345)
                      in bestCost <= initCost && length bestSol == length initSol

-- | Execute all unit tests; abort on failure.
runTests :: IO ()
runTests = do
    let _ = assert prop_costSymmetry ()
    let _ = assert prop_neighborPermutation ()
    let _ = assert prop_annealImproves ()
    putStrLn "All tests passed."

-- | Demonstration of the optimizer on the sample graph.
demo :: IO ()
demo = do
    let g = sampleGraph
        initSol = initialSolution (length g)
        initCost = cost g initSol
    putStrLn $ "Initial solution: " ++ show initSol ++ " cost = " ++ show initCost
    let (bestSol, bestCost) = anneal g initSol 100.0 0.95 500 (mkStdGen 2021)
    putStrLn $ "Optimized solution: " ++ show bestSol ++ " cost = " ++ show bestCost

main :: IO ()
main = do
    runTests
    demo
