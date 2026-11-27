module Main where

import Core
import System.Exit (exitFailure)

-- Simple assertion helper.
assert :: Bool -> String -> IO ()
assert True _   = return ()
assert False msg = putStrLn ("Assertion failed: " ++ msg) >> exitFailure

-- Test the DSU implementation.
testDSU :: IO ()
testDSU = do
    let dsu0 = emptyDSU [1,2,3]
        dsu1 = union dsu0 1 2
        (r1, dsu2) = find dsu1 1
        (r2, _)    = find dsu2 2
    assert (r1 == r2) "DSU union/find failed"

-- Test Kruskal on two small graphs.
testKruskal :: IO ()
testKruskal = do
    -- Triangle graph.
    let edges1 = [ Edge 1 0 1
                 , Edge 2 1 2
                 , Edge 3 0 2
                 ]
        (mst1, w1) = kruskalMST 3 edges1
    assert (length mst1 == 2) "MST edge count incorrect (graph 1)"
    assert (w1 == 3) "MST total weight incorrect (graph 1)"

    -- Square with diagonals.
    let edges2 = [ Edge 1 0 1
                 , Edge 4 0 2
                 , Edge 3 0 3
                 , Edge 2 1 2
                 , Edge 5 1 3
                 , Edge 6 2 3
                 ]
        (mst2, w2) = kruskalMST 4 edges2
    assert (length mst2 == 3) "MST edge count incorrect (graph 2)"
    assert (w2 == 6) "MST total weight incorrect (graph 2)"

main :: IO ()
main = do
    testDSU
    testKruskal
    putStrLn "All tests passed."
