module Main where

import Types
import Engine
import System.Random (mkStdGen)

-- Simple assertion helpers
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual testName expected actual =
  if expected == actual
  then putStrLn $ "[PASS] " ++ testName
  else putStrLn $ "[FAIL] " ++ testName ++ ": expected " ++ show expected ++ ", got " ++ show actual

assertTrue :: String -> Bool -> IO ()
assertTrue testName condition =
  if condition
  then putStrLn $ "[PASS] " ++ testName
  else putStrLn $ "[FAIL] " ++ testName

-- Test graphs
triangleGraph :: Graph
triangleGraph = [[1,2],[0,2],[0,1]]  -- 3-clique

bipartiteGraph :: Graph
bipartiteGraph = [[1,2],[0,3],[0,3],[1,2]]  -- 4-cycle

-- Expected optimal colors
expectedTriangleColors :: Int
expectedTriangleColors = 3

expectedBipartiteColors :: Int
expectedBipartiteColors = 2

-- Run simulated annealing with fixed seed for reproducibility
runSA :: Graph -> Int -> IO (Coloring, Int)
runSA graph maxColor = do
  let params = (1000.0, 0.95, 1.0, 10000)
      seed = 42
      gen = mkStdGen seed
      (bestColoring, bestConflicts, _) = simulatedAnnealing graph params maxColor gen
  return (bestColoring, bestConflicts)

-- Count distinct colors used
distinctColors :: Coloring -> Int
distinctColors = length . foldr (\c acc -> if c `elem` acc then acc else c:acc) []

main :: IO ()
main = do
  putStrLn "=== Simulated Annealing Graph Coloring Tests ==="

  -- Test 1: Triangle graph should need 3 colors
  (triColoring, triConflicts) <- runSA triangleGraph 3
  let triColorsUsed = distinctColors triColoring
  assertEqual "Triangle graph colors used" expectedTriangleColors triColorsUsed
  assertTrue "Triangle graph has no conflicts" (triConflicts == 0)

  -- Test 2: Bipartite graph should need 2 colors
  (biColoring, biConflicts) <- runSA bipartiteGraph 2
  let biColorsUsed = distinctColors biColoring
  assertEqual "Bipartite graph colors used" expectedBipartiteColors biColorsUsed
  assertTrue "Bipartite graph has no conflicts" (biConflicts == 0)

  -- Demo: Random graph
  let randomGraph = [[1,2],[0,3],[0,3,4],[1,2],[2]]
  putStrLn "\n=== Demo on Random Graph ==="
  (demoColoring, demoConflicts) <- runSA randomGraph 4
  putStrLn $ "Best coloring: " ++ show demoColoring
  putStrLn $ "Conflicts: " ++ show demoConflicts
  putStrLn $ "Colors used: " ++ show (distinctColors demoColoring)
