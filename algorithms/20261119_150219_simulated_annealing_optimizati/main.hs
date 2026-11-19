module Main where

import Types
import Engine
import System.Random (mkStdGen)

-- Simple assertion helper.
assert :: Bool -> String -> IO ()
assert True _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)

main :: IO ()
main = do
    -- Test graph: complete graph K4 with unit weights.
    let gK4 = Graph 4 [ (0,1,1.0), (0,2,1.0), (0,3,1.0)
                     , (1,2,1.0), (1,3,1.0)
                     , (2,3,1.0) ]
    let optimalScoreK4 = 4.0   -- Max‑cut of K4 is 4 edges (partition 2‑2)

    let seed = 42
    let gen0 = mkStdGen seed

    let (initSolK4, gen1) = randomSolution (numVertices gK4) gen0
    let (bestSolK4, bestScoreK4, _) = anneal gK4 initSolK4 10.0 0.95 100 gen1

    putStrLn $ "K4 best cut weight: " ++ show bestScoreK4
    assert (bestScoreK4 >= optimalScoreK4 - 1e-6)
           ("Expected at least " ++ show optimalScoreK4 ++ ", got " ++ show bestScoreK4)

    -- Edge case: empty graph.
    let gEmpty = Graph 0 []
    let (initEmpty, gen2) = randomSolution 0 gen1
    let (bestEmpty, bestEmptyScore, _) = anneal gEmpty initEmpty 5.0 0.9 10 gen2
    assert (bestEmptyScore == 0.0) "Empty graph must have cut weight 0"

    -- Edge case: single vertex graph.
    let gSingle = Graph 1 []
    let (initSingle, gen3) = randomSolution 1 gen2
    let (bestSingle, bestSingleScore, _) = anneal gSingle initSingle 5.0 0.9 10 gen3
    assert (bestSingleScore == 0.0) "Single‑vertex graph must have cut weight 0"

    putStrLn "All tests passed."
