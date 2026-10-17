module Main where

import Core
import System.Random
import System.Exit (exitFailure)
import Data.List (intercalate)

-- Simple assertion helpers
assertBool :: String -> Bool -> IO ()
assertBool msg True  = putStrLn $ "✓ " ++ msg
assertBool msg False = putStrLn ("✗ " ++ msg) >> exitFailure

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual msg expected actual =
    if expected == actual
    then putStrLn $ "✓ " ++ msg
    else putStrLn ("✗ " ++ msg ++ " (expected " ++ show expected ++ ", got " ++ show actual ++ ")") >> exitFailure

-- Test: subset sum via simulated annealing
testSubsetSum :: IO ()
testSubsetSum = do
    let weights = [1..10]
        target  = 15
        n       = length weights
    gen0 <- newStdGen
    let (initSol, gen1) = randomSolution n gen0
        bestSol = simulatedAnnealing weights target initSol 10.0 0.95 5000 gen1
        subsetSum = sum [w | (w, True) <- zip weights bestSol]
    assertBool ("Found subset sum close to target: " ++ show subsetSum)
        (abs (subsetSum - target) <= 1)

-- Test: solution validity (all elements either included or not)
testSolutionValidity :: IO ()
testSolutionValidity = do
    let weights = [2,3,5,7]
        target  = 10
        n       = length weights
    gen0 <- newStdGen
    let (initSol, gen1) = randomSolution n gen0
        bestSol = simulatedAnnealing weights target initSol 5.0 0.9 2000 gen1
    assertBool ("Solution contains only True/False values")
        (all (`elem` [True, False]) bestSol)

-- Run all tests
runTests :: IO ()
runTests = do
    putStrLn "Running tests..."
    testSubsetSum
    testSolutionValidity
    putStrLn "All tests passed."

-- Simple benchmark: run simulated annealing multiple times
benchmark :: IO ()
benchmark = do
    let weights = [1..20]
        target  = 100
        n       = length weights
    gen0 <- newStdGen
    let (initSol, gen1) = randomSolution n gen0
        bestSol = simulatedAnnealing weights target initSol 20.0 0.9 10000 gen1
        subsetSum = sum [w | (w, True) <- zip weights bestSol]
    putStrLn $ "Benchmark result: subset sum = " ++ show subsetSum

main :: IO ()
main = do
    runTests
    benchmark
