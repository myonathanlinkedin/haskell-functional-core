module Main where

import Types
import Engine
import System.Random (mkStdGen)
import Control.Exception (assert)
import Data.List (sort)

-- | Simple helper to create a complete undirected graph from a weight matrix.
completeGraph :: [[Double]] -> Graph
completeGraph mat = [ ((i,j), w) | i <- idx, j <- idx, i < j, let w = mat !! i !! j ]
  where idx = [0 .. length mat - 1]

-- | Known optimal tour for the test graph (0-1-2-3-0) with cost 4.0
testGraph :: Graph
testGraph = completeGraph
  [ [0,1,2,3]
  , [1,0,1,2]
  , [2,1,0,1]
  , [3,2,1,0]
  ]

testVertices :: [Vertex]
testVertices = [0,1,2,3]

optimalCost :: Double
optimalCost = 4.0

-- | Run a single test case and assert that SA finds the optimal cost.
runTest :: IO ()
runTest = do
  let initTour = testVertices
      params = SAParams
                { initialTemp = 10.0
                , finalTemp   = 0.001
                , alpha       = 0.85
                , iterPerTemp = 200
                }
      gen = mkStdGen 42   -- deterministic seed
      (bestTour, bestCost, _) = simulatedAnnealing testGraph initTour params gen
      _ = assert (abs (bestCost - optimalCost) < 1e-6) ()
  putStrLn $ "Test passed. Best tour: " ++ show bestTour ++ " Cost: " ++ show bestCost

-- | Demonstration on a slightly larger random-like graph.
demo :: IO ()
demo = do
  let mat = [ [0,2,9,10,7]
            , [2,0,6,4,3]
            , [9,6,0,8,5]
            , [10,4,8,0,6]
            , [7,3,5,6,0]
            ]
      g = completeGraph mat
      verts = [0..4]
      initTour = verts
      params = SAParams
                { initialTemp = 15.0
                , finalTemp   = 0.001
                , alpha       = 0.90
                , iterPerTemp = 300
                }
      gen = mkStdGen 12345
      (bestTour, bestCost, _) = simulatedAnnealing g initTour params gen
  putStrLn $ "Demo completed."
  putStrLn $ "Best tour found: " ++ show bestTour
  putStrLn $ "Tour cost: " ++ show bestCost

main :: IO ()
main = do
  runTest
  demo
