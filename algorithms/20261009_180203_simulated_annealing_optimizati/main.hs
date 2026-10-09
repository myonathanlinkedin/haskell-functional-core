module Main where

import Core
import System.Random (mkStdGen)
import Control.Monad (when)

-- Simple assertion helper
assert :: Bool -> String -> IO ()
assert True _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)

-- Test 1: Triangle graph with 3 colors (should be 0 conflicts)
testTriangle3Colors :: IO ()
testTriangle3Colors = do
    let g = Graph 3 [(0,1),(1,2),(0,2)]
        colors = 3
        (bestAssign, bestE) = anneal g colors 10.0 0.99 5000 (mkStdGen 42)
    putStrLn $ "Test1 best energy: " ++ show bestE ++ " assignment: " ++ show bestAssign
    assert (bestE == 0) "Triangle with 3 colors should have zero conflicts"

-- Test 2: Triangle graph with 2 colors (minimum conflicts = 1)
testTriangle2Colors :: IO ()
testTriangle2Colors = do
    let g = Graph 3 [(0,1),(1,2),(0,2)]
        colors = 2
        (bestAssign, bestE) = anneal g colors 10.0 0.99 5000 (mkStdGen 7)
    putStrLn $ "Test2 best energy: " ++ show bestE ++ " assignment: " ++ show bestAssign
    assert (bestE >= 1) "Triangle with 2 colors cannot be conflict‑free"
    assert (bestE <= 2) "Energy should not exceed number of edges (3) for this simple run"

-- Test 3: Square (cycle of 4) with 2 colors (should be 0 conflicts)
testSquare2Colors :: IO ()
testSquare2Colors = do
    let g = Graph 4 [(0,1),(1,2),(2,3),(3,0)]
        colors = 2
        (bestAssign, bestE) = anneal g colors 5.0 0.95 4000 (mkStdGen 123)
    putStrLn $ "Test3 best energy: " ++ show bestE ++ " assignment: " ++ show bestAssign
    assert (bestE == 0) "Even cycle with 2 colors should be properly colored"

-- Run all tests
runTests :: IO ()
runTests = do
    putStrLn "Running Simulated Annealing tests..."
    testTriangle3Colors
    testTriangle2Colors
    testSquare2Colors
    putStrLn "All tests passed."

main :: IO ()
main = runTests
