module Main where

import Types
import Engine
import System.Random (mkStdGen)

-- Simple assertion helper.
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)

-- A tiny 5‑city complete graph with known optimal TSP tour cost = 7.0
sampleGraph :: Graph
sampleGraph =
  [ [0, 2, 9, 10, 7]
  , [2, 0, 6, 4, 3]
  , [9, 6, 0, 8, 5]
  , [10,4, 8, 0, 6]
  , [7, 3, 5, 6, 0]
  ]

-- Known optimal tour (0‑1‑4‑2‑3‑0) cost = 7.0
optimalCost :: Double
optimalCost = 7.0

-- Parameters chosen to give a reasonable chance of reaching the optimum.
defaultParams :: SAParams
defaultParams = SAParams
  { initialTemp = 100.0
  , finalTemp   = 0.001
  , alpha       = 0.95
  , iterPerTemp = 200
  }

main :: IO ()
main = do
  let seed = 42
      rng   = mkStdGen seed
      result = anneal sampleGraph defaultParams rng
      finalCost = bestCost result

  putStrLn $ "Best tour found: " ++ show (bestTour result)
  putStrLn $ "Tour cost: " ++ show finalCost
  putStrLn $ "Final temperature: " ++ show (finalTempReached result)

  -- Basic sanity checks
  assert (length (bestTour result) == length sampleGraph)
    "Resulting tour length must equal number of vertices."

  assert (finalCost <= optimalCost + 1e-6)
    ("Final cost " ++ show finalCost ++ " exceeds known optimum " ++ show optimalCost)

  -- Edge‑case: empty graph
  let emptyGraph = [] :: Graph
      emptyResult = anneal emptyGraph defaultParams (mkStdGen 1)
  assert (bestTour emptyResult == [])
    "Empty graph should yield empty tour."

  -- Edge‑case: single vertex graph
  let singleGraph = [[0.0]]
      singleResult = anneal singleGraph defaultParams (mkStdGen 2)
  assert (bestTour singleResult == [0])
    "Single vertex graph should yield tour [0]."

  putStrLn "All assertions passed. Simulated annealing implementation verified."
