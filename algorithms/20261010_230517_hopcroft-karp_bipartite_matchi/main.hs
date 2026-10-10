module Main where

import Core
import System.Exit (exitFailure)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

type Test = (String, Int, Int, [(Int,Int)], Int)

tests :: [Test]
tests =
    [ ("Empty graph", 0, 0, [], 0)
    , ("No edges", 3, 3, [], 0)
    , ("Single edge", 1, 1, [(1,1)], 1)
    , ("Simple 2x2", 2, 2, [(1,1),(1,2),(2,1)], 2)
    , ("Path graph", 3, 3, [(1,2),(2,1),(2,3),(3,2)], 2)
    , ("Complete bipartite 3x4", 3, 4, [(i,j) | i<-[1..3], j<-[1..4]], 3)
    , ("Larger example", 4, 5,
        [(1,1),(1,2),(2,2),(2,3),(3,3),(3,4),(4,4),(4,5)], 4)
    ]

runTest :: Test -> IO Bool
runTest (name, nL, nR, edges, expected) = do
    let result = maxMatchingSize nL nR edges
    if result == expected
        then do
            putStrLn $ "[PASS] " ++ name ++ " (size=" ++ show result ++ ")"
            return True
        else do
            putStrLn $ "[FAIL] " ++ name ++ " expected " ++ show expected ++ " but got " ++ show result
            return False

runAll :: IO ()
runAll = do
    results <- mapM runTest tests
    unless (and results) exitFailure

benchmark :: IO ()
benchmark = do
    let nL = 500
        nR = 500
        edges = [(i, j) | i <- [1..nL], j <- [1..nR], (i + j) `mod` 7 == 0]
    start <- getCPUTime
    let size = maxMatchingSize nL nR edges
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12) :: Double
    printf "Benchmark: %d vertices each side, %d edges, matching size %d, time %.3f sec\n"
        nL (length edges) size diff

main :: IO ()
main = do
    putStrLn "Running Hopcroft‑Karp unit tests..."
    runAll
    putStrLn "All tests passed."
    putStrLn "Running simple benchmark..."
    benchmark
