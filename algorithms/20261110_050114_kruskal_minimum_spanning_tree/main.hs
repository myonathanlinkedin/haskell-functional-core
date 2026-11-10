module Main where

import Types
import Engine (kruskalMST)
import System.Exit (exitFailure)

-- | Simple assertion helper.
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual msg expected actual =
  if expected == actual
    then putStrLn $ "[PASS] " ++ msg
    else do
      putStrLn $ "[FAIL] " ++ msg
      putStrLn $ "  Expected: " ++ show expected
      putStrLn $ "  Actual  : " ++ show actual
      exitFailure

-- | Test graph (the classic example with 6 vertices).
testGraph :: Graph
testGraph =
  [ Edge 0 1 4
  , Edge 0 2 4
  , Edge 1 2 2
  , Edge 2 3 3
  , Edge 2 4 2
  , Edge 2 5 4
  , Edge 3 4 3
  , Edge 4 5 3
  , Edge 3 5 2
  ]

-- Expected MST weight for the above graph is 11 and contains exactly 5 edges.
expectedMSTWeight :: Int
expectedMSTWeight = 11

expectedEdgeCount :: Int
expectedEdgeCount = 5

main :: IO ()
main = do
  let (mstEdges, totalWeight) = kruskalMST 6 testGraph
  assertEqual "MST total weight" expectedMSTWeight totalWeight
  assertEqual "MST edge count" expectedEdgeCount (length mstEdges)

  -- Additional sanity check: the MST must be acyclic and connect all vertices.
  let verticesInMST = foldr (\e acc -> from e : to e : acc) [] mstEdges
  let uniqueVertices = length (removeDuplicates verticesInMST)
  assertEqual "MST connects all vertices" 6 uniqueVertices

  putStrLn "All tests passed."

-- Helper to remove duplicate vertices.
removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = go []
  where
    go seen [] = seen
    go seen (x:xs)
      | x `elem` seen = go seen xs
      | otherwise     = go (x:seen) xs
