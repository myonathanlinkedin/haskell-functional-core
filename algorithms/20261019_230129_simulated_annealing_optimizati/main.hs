module Main where

import Core
import System.Random (mkStdGen)
import Data.Array (listArray)
import Text.Printf (printf)

-- Simple assertion helper
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)

-- Test 1: Square graph – optimal tour length is 4.0
testSquare :: IO ()
testSquare = do
    let pts = [ (0.0, 0.0)
              , (0.0, 1.0)
              , (1.0, 1.0)
              , (1.0, 0.0)
              ]
        graph = listArray (0,3) pts
        params = SAParams
                    { initialTemp = 10.0
                    , coolingRate = 0.95
                    , iterationsPerTemp = 500
                    , minTemp = 0.001
                    }
        seed = mkStdGen 12345
        (solution, cost, _) = anneal graph params seed
        expectedUpper = 4.5   -- allow small slack
    printf "Square test – final cost: %.4f\n" cost
    assert (cost <= expectedUpper) ("Cost " ++ show cost ++ " exceeds " ++ show expectedUpper)

-- Test 2: Determinism with fixed seed
testDeterminism :: IO ()
testDeterminism = do
    let pts = [ (0.0,0.0), (1.0,0.0), (0.0,1.0), (1.0,1.0), (0.5,0.5) ]
        graph = listArray (0,4) pts
        params = SAParams 5.0 0.9 300 0.0001
        seed = mkStdGen 999
        (sol1, cost1, gen1) = anneal graph params seed
        (sol2, cost2, _)    = anneal graph params seed
    assert (sol1 == sol2) "Solutions differ on identical seeds"
    assert (abs (cost1 - cost2) < 1e-12) "Costs differ on identical seeds"
    printf "Determinism test – cost: %.4f, solution: %s\n" cost1 (show sol1)

-- Test 3: Larger random graph (10 points) – ensure algorithm terminates
testLarger :: IO ()
testLarger = do
    let pts = [ (fromIntegral i, fromIntegral (i*i `mod` 11)) | i <- [0..9] ]
        graph = listArray (0,9) pts
        params = SAParams 20.0 0.93 800 0.001
        seed = mkStdGen 2021
        (_, cost, _) = anneal graph params seed
    printf "Larger graph test – final cost: %.4f\n" cost
    assert (cost > 0) "Cost should be positive"

main :: IO ()
main = do
    putStrLn "Running Simulated Annealing tests..."
    testSquare
    testDeterminism
    testLarger
    putStrLn "All tests passed."
