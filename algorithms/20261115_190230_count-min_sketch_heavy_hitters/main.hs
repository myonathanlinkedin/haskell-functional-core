module Main where

import Core
import System.Exit (exitFailure)

-- Simple assertion helper
assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False msg = putStrLn ("Assertion failed: " ++ msg) >> exitFailure

-- Test 1: basic insertion and estimation bounds
testBasic :: IO ()
testBasic = do
    let depth = 3
        width = 100
        seeds = [(1,2),(3,5),(7,11)]
        cms0 = mkCMS depth width seeds :: CountMinSketch Int
        -- Insert items: 1 appears 1000 times, 2 appears 200 times, 3 appears 50 times
        cms1 = foldl' add cms0 (replicate 1000 1)
        cms2 = foldl' add cms1 (replicate 200 2)
        cms3 = foldl' add cms2 (replicate 50 3)

        total = 1250 :: Int
        epsilon = total `div` width + 1   -- upper error bound (ceil)

        est1 = estimate cms3 1
        est2 = estimate cms3 2
        est3 = estimate cms3 3

    assert (est1 >= 1000) "est1 lower bound"
    assert (est1 <= 1000 + epsilon) "est1 upper bound"
    assert (est2 >= 200) "est2 lower bound"
    assert (est2 <= 200 + epsilon) "est2 upper bound"
    assert (est3 >= 50) "est3 lower bound"
    assert (est3 <= 50 + epsilon) "est3 upper bound"

-- Test 2: heavy hitters detection
testHeavyHitters :: IO ()
testHeavyHitters = do
    let depth = 4
        width = 200
        seeds = [(2,3),(5,7),(11,13),(17,19)]
        cms = mkCMS depth width seeds :: CountMinSketch Int
        cms' = foldl' add cms (replicate 800 42 ++ replicate 300 7 ++ replicate 100 99)

        threshold = 250
        hitters = heavyHitters cms' threshold [42,7,99,1,2]

        expected = [(42, estimate cms' 42), (7, estimate cms' 7)]
        got = filter (\(i,_) -> i `elem` [42,7]) hitters

    assert (length hitters == 2) "Exactly two heavy hitters expected"
    assert (all (`elem` map fst expected) (map fst hitters)) "Hitters set mismatch"

-- Test 3: deterministic hashing (same seeds produce same estimates)
testDeterminism :: IO ()
testDeterminism = do
    let depth = 2
        width = 50
        seeds = [(3,4),(6,8)]
        cmsA = mkCMS depth width seeds :: CountMinSketch Int
        cmsB = mkCMS depth width seeds :: CountMinSketch Int
        cmsA' = foldl' add cmsA (replicate 400 5 ++ replicate 150 9)
        cmsB' = foldl' add cmsB (replicate 400 5 ++ replicate 150 9)

    assert (estimate cmsA' 5 == estimate cmsB' 5) "Deterministic estimate for 5"
    assert (estimate cmsA' 9 == estimate cmsB' 9) "Deterministic estimate for 9"

-- Run all tests
runTests :: IO ()
runTests = do
    testBasic
    testHeavyHitters
    testDeterminism
    putStrLn "All tests passed."

main :: IO ()
main = runTests
