module Main where

import Core
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure)

-- Simple assertion helper.
assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False msg = putStrLn ("Assertion failed: " ++ msg) >> exitFailure

-- Test 1: basic 5x5 grid with a static obstacle.
testBasic :: IO ()
testBasic = do
    let rows = [ [1,1,1,1,1]
               , [1,-1,-1,-1,1]
               , [1,1,1,1,1]
               , [1,-1,1,-1,1]
               , [1,1,1,1,1] ]
        grid = mkGrid rows
        dynCost _ = 0
        start = (0,0)
        goal  = (4,4)
        result = aStar grid dynCost start goal
    case result of
        Nothing -> assert False "Basic test: path not found"
        Just path -> do
            -- Path must start and end correctly.
            assert (head path == start) "Basic test: start mismatch"
            assert (last path == goal)  "Basic test: goal mismatch"
            -- Path length should be 9 (minimum Manhattan distance + detours)
            assert (length path == 9) ("Basic test: unexpected path length " ++ show (length path))

-- Test 2: dynamic obstacle cost makes the algorithm avoid a high‑cost cell.
testDynamicCost :: IO ()
testDynamicCost = do
    let rows = replicate 5 (replicate 5 1)   -- uniform cost 1
        grid = mkGrid rows
        -- Add a high dynamic cost at (2,2)
        dynCost pos = if pos == (2,2) then 10 else 0
        start = (0,0)
        goal  = (4,4)
        result = aStar grid dynCost start goal
    case result of
        Nothing -> assert False "Dynamic cost test: path not found"
        Just path -> do
            assert (notElem (2,2) path) "Dynamic cost test: high‑cost cell not avoided"
            -- Verify total cost is minimal (should be 8, the Manhattan distance)
            let totalCost = sum [ fromMaybe 0 (grid p) + dynCost p | p <- path ]
            assert (totalCost == 8) ("Dynamic cost test: unexpected total cost " ++ show totalCost)

-- Test 3: unreachable goal.
testUnreachable :: IO ()
testUnreachable = do
    let rows = [ [1, -1, 1]
               , [1, -1, 1]
               , [1, -1, 1] ]
        grid = mkGrid rows
        dynCost _ = 0
        start = (0,0)
        goal  = (2,0)
        result = aStar grid dynCost start goal
    assert (result == Nothing) "Unreachable test: expected Nothing"

main :: IO ()
main = do
    putStrLn "Running A* pathfinding tests..."
    testBasic
    putStrLn "  Basic test passed."
    testDynamicCost
    putStrLn "  Dynamic cost test passed."
    testUnreachable
    putStrLn "  Unreachable test passed."
    putStrLn "All tests succeeded."
