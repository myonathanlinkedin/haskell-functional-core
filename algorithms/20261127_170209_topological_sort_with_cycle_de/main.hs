module Main where

import Types
import Engine (topologicalSort)
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure)

-- Simple assertion helper.
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual msg expected actual =
    if expected == actual
        then putStrLn $ "[PASS] " ++ msg
        else do
            putStrLn $ "[FAIL] " ++ msg
            putStrLn $ "  Expected: " ++ show expected
            putStrLn $ "  Actual:   " ++ show actual
            exitFailure

-- Test 1: Empty graph.
testEmpty :: IO ()
testEmpty = do
    let g = Map.empty :: Graph
    assertEqual "Empty graph" (Right []) (topologicalSort g)

-- Test 2: Single vertex, no edges.
testSingle :: IO ()
testSingle = do
    let g = Map.fromList [(1, [])] :: Graph
    assertEqual "Single vertex" (Right [1]) (topologicalSort g)

-- Test 3: Simple DAG.
testDAG :: IO ()
testDAG = do
    -- 5 → 2 → 3
    -- 5 → 0
    -- 4 → 0
    -- 4 → 1
    -- 2 → 1
    let g = Map.fromList
                [ (5, [2,0])
                , (4, [0,1])
                , (2, [3,1])
                , (3, [])
                , (1, [])
                , (0, [])
                ]
    let result = topologicalSort g
    case result of
        Left err -> error $ "Unexpected cycle detection: " ++ err
        Right order -> do
            -- Verify that for every edge u→v, u appears before v.
            let edges = [(5,2),(5,0),(4,0),(4,1),(2,3),(2,1)]
                position x = case elemIndex x order of
                               Just i  -> i
                               Nothing -> error $ "Vertex " ++ show x ++ " missing from order"
                elemIndex = \x xs -> lookup x (zip xs [0..])
            mapM_ (\(u,v) ->
                assertEqual ("Edge " ++ show u ++ "→" ++ show v ++ " respects order")
                    True (position u < position v))
                edges

-- Test 4: Graph with a cycle.
testCycle :: IO ()
testCycle = do
    -- 1 → 2 → 3 → 1 (cycle)
    let g = Map.fromList
                [ (1, [2])
                , (2, [3])
                , (3, [1])
                ]
    assertEqual "Cycle detection" (Left "Graph contains a cycle; topological order not possible.") (topologicalSort g)

-- Test 5: Disconnected DAG.
testDisconnected :: IO ()
testDisconnected = do
    let g = Map.fromList
                [ (1, [2])
                , (2, [])
                , (3, [4])
                , (4, [])
                ]
    let result = topologicalSort g
    case result of
        Left err -> error $ "Unexpected cycle detection: " ++ err
        Right order -> do
            -- Ensure each component respects its own edges.
            let check u v = assertEqual ("Edge " ++ show u ++ "→" ++ show v ++ " respects order")
                                        True (elemIndex u order < elemIndex v order)
                elemIndex x xs = case lookup x (zip xs [0..]) of
                                   Just i  -> i
                                   Nothing -> error $ "Vertex " ++ show x ++ " missing"
            check 1 2
            check 3 4

main :: IO ()
main = do
    putStrLn "Running Topological Sort tests..."
    testEmpty
    testSingle
    testDAG
    testCycle
    testDisconnected
    putStrLn "All tests passed."
