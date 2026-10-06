module Main where

import Core
import System.Random
import Data.List (sort, sortBy)
import Data.Ord (comparing)

-- | Unit test: Verify energy calculation
testEnergyCalculation :: IO Bool
testEnergyCalculation = do
  let graph = createSampleGraph
      solution = [0, 1, 2, 3, 4]
      expectedEnergy = 4.0  -- Path 0-1-2-3-4 has edges 0-1, 1-2, 2-3, 3-4
      actualEnergy = calculateEnergy graph solution
  putStrLn $ "Test Energy Calculation: " ++ show (actualEnergy == expectedEnergy)
  return (actualEnergy == expectedEnergy)

-- | Unit test: Verify neighbor generation
testNeighborGeneration :: IO Bool
testNeighborGeneration = do
  let solution = [0, 1, 2, 3, 4]
      gen = mkStdGen 42
      (neighbor, _) = generateNeighbor solution gen
  let isValid = length neighbor == length solution && all (\x -> x `elem` solution) neighbor
  putStrLn $ "Test Neighbor Generation: " ++ show isValid
  return isValid

-- | Unit test: Verify SA initialization
testSAInitialization :: IO Bool
testSAInitialization = do
  let graph = createSampleGraph
      config = SAConfig 100.0 0.995 1000 42
      (state, _) = initializeSA graph config
  let isValid = currentSolution state == [0, 1, 2, 3, 4] && currentEnergy state > 0
  putStrLn $ "Test SA Initialization: " ++ show isValid
  return isValid

-- | Unit test: Verify SA runs and produces valid solution
testSARun :: IO Bool
testSARun = do
  let graph = createSampleGraph
      config = SAConfig 100.0 0.995 500 42
      (solution, energy) = runSimulatedAnnealing graph config
  let isValid = length solution == 5 && all (\x -> x `elem` [0, 1, 2, 3, 4]) solution && energy > 0
  putStrLn $ "Test SA Run: " ++ show isValid
  putStrLn $ "  Solution: " ++ show solution
  putStrLn $ "  Energy: " ++ show energy
  return isValid

-- | Unit test: Verify SA finds better solution than initial
testSABetterThanInitial :: IO Bool
testSABetterThanInitial = do
  let graph = createSampleGraph
      config = SAConfig 100.0 0.995 1000 42
      initialSol = [0, 1, 2, 3, 4]
      initialEnergy = calculateEnergy graph initialSol
      (solution, energy) = runSimulatedAnnealing graph config
  let improved = energy <= initialEnergy
  putStrLn $ "Test SA Better Than Initial: " ++ show improved
  putStrLn $ "  Initial Energy: " ++ show initialEnergy
  putStrLn $ "  Final Energy: " ++ show energy
  return improved

-- | Benchmark: Run SA with different parameters
benchmarkSA :: IO ()
benchmarkSA = do
  let graph = createSampleGraph
      configs = [SAConfig 50.0 0.99 100 1,
                 SAConfig 100.0 0.995 500 2,
                 SAConfig 200.0 0.999 1000 3]
  mapM_ (\config -> do
    let (solution, energy) = runSimulatedAnnealing graph config
    putStrLn $ "Config: " ++ show config
    putStrLn $ "  Solution: " ++ show solution
    putStrLn $ "  Energy: " ++ show energy
    putStrLn ""
  ) configs

-- | Main entry point
main :: IO ()
main = do
  putStrLn "=== Simulated Annealing Optimization for Combinatorial Graphs ==="
  putStrLn ""

  putStrLn "--- Running Unit Tests ---"
  test1 <- testEnergyCalculation
  test2 <- testNeighborGeneration
  test3 <- testSAInitialization
  test4 <- testSARun
  test5 <- testSABetterThanInitial

  putStrLn ""
  putStrLn "--- Test Results ---"
  putStrLn $ "Energy Calculation: " ++ (if test1 then "PASS" else "FAIL")
  putStrLn $ "Neighbor Generation: " ++ (if test2 then "PASS" else "FAIL")
  putStrLn $ "SA Initialization: " ++ (if test3 then "PASS" else "FAIL")
  putStrLn $ "SA Run: " ++ (if test4 then "PASS" else "FAIL")
  putStrLn $ "SA Better Than Initial: " ++ (if test5 then "PASS" else "FAIL")

  let allPassed = test1 && test2 && test3 && test4 && test5
  putStrLn ""
  putStrLn $ "All Tests: " ++ (if allPassed then "PASSED" else "FAILED")

  putStrLn ""
  putStrLn "--- Running Benchmarks ---"
  benchmarkSA

  putStrLn ""
  putStrLn "=== Complete ==="
