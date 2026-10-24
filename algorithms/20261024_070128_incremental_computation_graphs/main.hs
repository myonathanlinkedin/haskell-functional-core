module Main where

import Core
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import System.Exit (exitFailure)

-- Simple assertion helper
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual msg expected actual =
    if expected == actual
        then putStrLn $ "[PASS] " ++ msg
        else do
            putStrLn $ "[FAIL] " ++ msg
            putStrLn $ "  Expected: " ++ show expected
            putStrLn $ "  Actual:   " ++ show actual
            exitFailure

-- Build a sample graph:
--   a (input)   b (input)
--   c = a + b
--   d = c * 2
--   e = d - a
sampleGraph :: Graph Int
sampleGraph = foldl' (\g (nid, deps, f) -> addNode nid deps f g) emptyGraph nodes
  where
    nodes =
        [ (1, [], const 0)                         -- placeholder for input a
        , (2, [], const 0)                         -- placeholder for input b
        , (3, [1,2], \m -> (m Map.! 1) + (m Map.! 2))   -- c = a + b
        , (4, [3],   \m -> (m Map.! 3) * 2)             -- d = c * 2
        , (5, [4,1], \m -> (m Map.! 4) - (m Map.! 1))   -- e = d - a
        ]

-- Helper to set input values (overwrites placeholder nodes 1 and 2)
setInputs :: Int -> Int -> Map NodeId Int
setInputs a b = Map.fromList [(1,a),(2,b)]

testFullEvaluation :: IO ()
testFullEvaluation = do
    let inputs = setInputs 1 2
        Right vals = evaluate sampleGraph inputs
        expected = Map.fromList [(1,1),(2,2),(3,3),(4,6),(5,5)]
    assertEqual "Full evaluation" expected vals

testIncrementalUpdate :: IO ()
testIncrementalUpdate = do
    let initInputs = setInputs 1 2
        Right initVals = evaluate sampleGraph initInputs
        changed = Map.fromList [(1,3)]  -- change a from 1 to 3
        Right newVals = incrementalUpdate sampleGraph initVals changed
        expected = Map.fromList [(1,3),(2,2),(3,5),(4,10),(5,7)]
    assertEqual "Incremental update after changing a" expected newVals

testCycleDetection :: IO ()
testCycleDetection = do
    let cyclic = addNode 6 [6] (\m -> m Map.! 6) emptyGraph
        result = topologicalOrder cyclic
    case result of
        Left _ -> putStrLn "[PASS] Cycle detection"
        Right _ -> do
            putStrLn "[FAIL] Cycle detection: expected failure"
            exitFailure

main :: IO ()
main = do
    testFullEvaluation
    testIncrementalUpdate
    testCycleDetection
    putStrLn "All tests passed."
