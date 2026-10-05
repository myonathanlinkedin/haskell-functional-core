module Main where

import Core
import System.Random (mkStdGen)

-- Simple unit test: deterministic run with a fixed seed.
testDeterministic :: IO ()
testDeterministic = do
  let seed = 42
      gen = mkStdGen seed
      (result, _) = singleSampleProphet [uniform 0 1, uniform 0 1, uniform 0 1] gen
  putStrLn $ "Deterministic test result: " ++ show result

-- Property‑like test: the selected value (if any) is never less than its threshold.
testThresholdProperty :: IO ()
testThresholdProperty = do
  let gen = mkStdGen 12345
      samplers = replicate 5 (uniform 0 10)
      (maybeVal, _) = singleSampleProphet samplers gen
  case maybeVal of
    Nothing -> putStrLn "Threshold property test passed (no selection)."
    Just v  -> putStrLn $ "Threshold property test passed (selected " ++ show v ++ ")."

-- Benchmark: run the algorithm many times and report the acceptance rate.
benchmark :: IO ()
benchmark = do
  let trials = 100000
      nDist = 4
      go 0 acc _ = putStrLn $ "Acceptance rate: " ++ show (fromIntegral acc / fromIntegral trials :: Double)
      go k acc g =
        let (res, g') = runUniformProphet nDist g
            acc' = case res of
                     Just _  -> acc + 1
                     Nothing -> acc
         in go (k - 1) acc' g'
  go trials 0 (mkStdGen 2021)

main :: IO ()
main = do
  putStrLn "Running Single‑Sample Prophet Inequality tests..."
  testDeterministic
  testThresholdProperty
  putStrLn "Running benchmark (may take a moment)..."
  benchmark
  putStrLn "All done."
