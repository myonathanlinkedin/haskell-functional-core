module Main where

import Core
import System.Random (mkStdGen)
import Test.HUnit
import Data.List (permutations, minimumBy)
import Data.Ord (comparing)

-- | Small complete graph with 5 nodes and symmetric integer weights.
sampleGraph :: Graph
sampleGraph = makeGraph 5
    [ (0,1,2), (0,2,9), (0,3,10), (0,4,7)
    , (1,2,6), (1,3,4), (1,4,3)
    , (2,3,8), (2,4,5)
    , (3,4,1)
    ]

-- Brute‑force optimal tour length (node 0 fixed at start).
optimalLength :: Int
optimalLength = minimum [ tourEnergy sampleGraph (0:perm) | perm <- permutations [1..4] ]

-- Test that the energy function matches manual calculation.
testEnergy :: Test
testEnergy = TestCase $ do
    let tour = [0,1,2,3,4]
        expected = sum [2,6,8,1,7]  -- edges (0-1,1-2,2-3,3-4,4-0)
    assertEqual "tourEnergy matches manual sum" expected (tourEnergy sampleGraph tour)

-- Test that simulated annealing finds the optimal length on this tiny instance.
testSAOptimal :: Test
testSAOptimal = TestCase $ do
    let seed = 42
        gen = mkStdGen seed
        (finalTour, _) = simulatedAnnealing sampleGraph 100.0 0.99 50000 gen
        finalLen = tourEnergy sampleGraph finalTour
    assertEqual "SA reaches optimal length" optimalLength finalLen

-- Aggregate tests.
tests :: Test
tests = TestList [ TestLabel "Energy" testEnergy
                 , TestLabel "SimulatedAnnealingOptimal" testSAOptimal
                 ]

main :: IO ()
main = do
    _ <- runTestTT tests
    putStrLn "All tests completed."
