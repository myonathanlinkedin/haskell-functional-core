module Main where

import Types
import Engine
import qualified Data.IntMap.Strict as IM
import System.Random (mkStdGen)

-- | Test that the algorithm finds the optimal cut (size 2) on a triangle graph.
testTriangle :: IO ()
testTriangle = do
    let graph = [(1,2),(2,3),(1,3)] :: Graph
        vertices = [1,2,3]
        initAssign = initialAssignment vertices
        params = SAParams
            { initialTemp = 10.0
            , coolingRate = 0.9
            , stepsPerTemp = 200
            , minTemp = 0.1
            }
        gen = mkStdGen 42
        (finalAssign, _) = anneal graph params initAssign gen
        finalScore = evaluate graph finalAssign
    if finalScore == 2
        then putStrLn "Triangle test passed."
        else error $ "Triangle test failed: expected cut size 2, got " ++ show finalScore

-- | Test that an empty graph yields a cut size of 0.
testEmpty :: IO ()
testEmpty = do
    let graph = [] :: Graph
        vertices = [] :: [Vertex]
        initAssign = initialAssignment vertices
        params = SAParams
            { initialTemp = 5.0
            , coolingRate = 0.8
            , stepsPerTemp = 10
            , minTemp = 0.1
            }
        gen = mkStdGen 1
        (finalAssign, _) = anneal graph params initAssign gen
        finalScore = evaluate graph finalAssign
    if finalScore == 0
        then putStrLn "Empty graph test passed."
        else error $ "Empty graph test failed: expected cut size 0, got " ++ show finalScore

-- | Demonstration on a small graph.
demo :: IO ()
demo = do
    let graph = [(1,2),(2,3),(3,4),(4,1),(1,3)] :: Graph
        vertices = [1,2,3,4]
        initAssign = initialAssignment vertices
        params = SAParams
            { initialTemp = 8.0
            , coolingRate = 0.85
            , stepsPerTemp = 150
            , minTemp = 0.1
            }
        gen = mkStdGen 12345
        (bestAssign, _) = anneal graph params initAssign gen
        bestScore = evaluate graph bestAssign
    putStrLn $ "Demo graph cut size: " ++ show bestScore
    putStrLn $ "Assignment: " ++ show (IM.toList bestAssign)

main :: IO ()
main = do
    testTriangle
    testEmpty
    demo
    putStrLn "All tests passed."
