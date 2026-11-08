module Main where

import Core (ODE, rk4Step, rk4Integrate)
import System.Exit (exitFailure)

-- | Absolute tolerance used for floating‑point comparisons in tests.
epsilon :: Double
epsilon = 1e-9

-- | Helper to compare two Doubles within a tolerance.
(~=) :: Double -> Double -> Bool
a ~= b = abs (a - b) <= epsilon

-- | Test 1: Exponential growth dy/dt = y, solution y = y0 * exp(t)
testExpGrowth :: IO ()
testExpGrowth = do
    let f :: ODE
        f _ y = y
        t0 = 0.0
        y0 = 1.0
        tEnd = 1.0
        h = 0.1
        result = rk4Integrate f t0 y0 tEnd h
        (_, yApprox) = last result
        yExact = y0 * exp (tEnd - t0)
    if yApprox ~= yExact
        then putStrLn "testExpGrowth passed."
        else do
            putStrLn $ "testExpGrowth failed: approx=" ++ show yApprox ++
                       ", exact=" ++ show yExact
            exitFailure

-- | Test 2: Exponential decay dy/dt = -y, solution y = y0 * exp(-t)
testExpDecay :: IO ()
testExpDecay = do
    let f :: ODE
        f _ y = -y
        t0 = 0.0
        y0 = 2.0
        tEnd = 2.0
        h = 0.05
        result = rk4Integrate f t0 y0 tEnd h
        (_, yApprox) = last result
        yExact = y0 * exp (-(tEnd - t0))
    if yApprox ~= yExact
        then putStrLn "testExpDecay passed."
        else do
            putStrLn $ "testExpDecay failed: approx=" ++ show yApprox ++
                       ", exact=" ++ show yExact
            exitFailure

-- | Test 3: Single step consistency with rk4Step.
testSingleStep :: IO ()
testSingleStep = do
    let f :: ODE
        f t y = t + y
        t = 0.0
        y = 1.0
        h = 0.2
        yStep = rk4Step f t y h
        -- Integrate with a single step using rk4Integrate and compare.
        [(t0, y0), (t1, y1)] = rk4Integrate f t y (t + h) h
    if yStep ~= y1 && t0 == t && t1 == t + h
        then putStrLn "testSingleStep passed."
        else do
            putStrLn $ "testSingleStep failed: step=" ++ show yStep ++
                       ", integrate=" ++ show y1
            exitFailure

-- | Run all tests and report the outcome.
runTests :: IO ()
runTests = do
    testExpGrowth
    testExpDecay
    testSingleStep
    putStrLn "All tests passed."

main :: IO ()
main = runTests
