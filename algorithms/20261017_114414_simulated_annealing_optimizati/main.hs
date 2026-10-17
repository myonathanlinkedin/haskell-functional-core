module Main where

import Core (Graph, anneal)
import System.Random (mkStdGen)

-- A tiny 4‑node graph where the optimal Hamiltonian cycle has total weight 4.
testGraph :: Graph
testGraph =
  [ [0, 1, 10, 1]
  , [1, 0, 1, 10]
  , [10, 1, 0, 1]
  , [1, 10, 1, 0]
  ]

expectedCost :: Double
expectedCost = 4.0

runTest :: IO ()
runTest =
  let (solution, costVal) = anneal testGraph
                                   10.0   -- initial temperature
                                   0.001  -- final temperature
                                   0.9    -- cooling factor
                                   100    -- iterations per temperature
                                   (mkStdGen 42)   -- deterministic seed
  in if abs (costVal - expectedCost) < 1e-6
        then putStrLn $ "Test passed. Cost = " ++ show costVal ++ ", solution = " ++ show solution
        else error $ "Test failed. Obtained cost = " ++ show costVal ++ ", expected " ++ show expectedCost

main :: IO ()
main = runTest
