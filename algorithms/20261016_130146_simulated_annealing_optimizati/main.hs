module Main where

import Types
import Engine
import System.Random (mkStdGen)

-- | A small symmetric graph (5 vertices) used for testing.
sampleGraph :: Graph
sampleGraph =
  [ [0, 2, 9,10, 7]
  , [2, 0, 6, 4, 3]
  , [9, 6, 0, 8, 5]
  , [10,4, 8, 0, 6]
  , [7, 3, 5, 6, 0]
  ]

-- | Starting tour (identity permutation).
initialTour :: Tour
initialTour = Tour [0,1,2,3,4]

-- | Simulated‑annealing parameters tuned for deterministic testing.
params :: SAParams
params = SAParams
  { initialTemp = 1000.0
  , finalTemp   = 0.1
  , alpha       = 0.95
  , iterPerTemp = 200
  }

-- | Simple assertion helper.
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)

main :: IO ()
main = do
  let gen = mkStdGen 42               -- deterministic seed
      (bestTour, bestCost) = runSA sampleGraph params gen initialTour

  putStrLn $ "Best tour found: " ++ show (unTour bestTour)
  putStrLn $ "Tour cost: " ++ show bestCost

  -- Known best cost for this graph (exhaustive search) is 26.0.
  -- The algorithm with the fixed seed must achieve a cost ≤ 30.0.
  let acceptableCost = 30.0
  assert (bestCost <= acceptableCost)
         ("Resulting cost " ++ show bestCost ++ " exceeds acceptable bound " ++ show acceptableCost)
