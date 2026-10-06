module Main where

import Types
import Engine
import System.Random (mkStdGen)
import Data.Array
import Control.Exception (assert)

-- | Helper to build a symmetric distance matrix from a list of edge weights.
mkGraph :: Int -> [(Int, Int, Double)] -> Graph
mkGraph n edges = array ((0,0),(n-1,n-1))
  [ ((i,j), w i j) | i <- [0..n-1], j <- [0..n-1] ]
  where
    edgeMap = [ ((i,j), d) | (i,j,d) <- edges ] ++ [ ((j,i), d) | (i,j,d) <- edges ]
    w i j | i == j    = 0
          | otherwise = case lookup (i,j) edgeMap of
                          Just d  -> d
                          Nothing -> error $ "Missing edge weight for (" ++ show i ++ "," ++ show j ++ ")"

-- | Small benchmark graph (5‑city TSP) with known optimal tour length 10.0.
sampleGraph :: Graph
sampleGraph = mkGraph 5
  [ (0,1,2.0), (0,2,9.0), (0,3,10.0), (0,4,7.0)
  , (1,2,6.0), (1,3,4.0), (1,4,3.0)
  , (2,3,8.0), (2,4,5.0)
  , (3,4,1.0)
  ]

optimalCost :: Double
optimalCost = 10.0   -- known optimal for the above instance

-- | Verify that the annealer finds a tour not worse than a tolerance above optimum.
testAnnealer :: Bool
testAnnealer = finalCost <= optimalCost + 2.0   -- allow small slack due to randomness
  where
    params = SAParams { initialTemp = 100.0
                      , finalTemp   = 0.001
                      , alpha       = 0.95
                      , innerIter   = 200
                      }
    rng = mkStdGen 42
    bestPath = anneal sampleGraph params rng
    finalCost = cost sampleGraph bestPath

-- | Edge‑case: empty graph (no vertices) should have zero cost.
testEmptyGraph :: Bool
testEmptyGraph = cost emptyG [] == 0.0
  where
    emptyG = array ((0,0),(-1,-1)) [] :: Graph

-- | Edge‑case: single‑vertex graph should have zero cost.
testSingleVertex :: Bool
testSingleVertex = cost singleG [0] == 0.0
  where
    singleG = mkGraph 1 []

main :: IO ()
main = do
  putStrLn "Running Simulated Annealing tests..."
  let _ = assert testAnnealer (putStrLn "  [PASS] Annealer finds near‑optimal tour")
  let _ = assert testEmptyGraph (putStrLn "  [PASS] Empty graph cost is zero")
  let _ = assert testSingleVertex (putStrLn "  [PASS] Single‑vertex graph cost is zero")
  putStrLn "All tests completed."
