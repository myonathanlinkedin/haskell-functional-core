module Main where

import Core
import qualified Data.IntMap.Strict as IM
import System.Exit (exitFailure)

-- Helper to build adjacency list from list of edges (u,v)
buildAdj :: Int -> [(Int,Int)] -> AdjList
buildAdj nU edges = IM.fromListWith (++) [(u, [v]) | (u,v) <- edges]

-- Simple assertion helper
assert :: Bool -> String -> IO ()
assert True _  = return ()
assert False msg = putStrLn ("Assertion failed: " ++ msg) >> exitFailure

-- Test cases
test1 :: IO ()
test1 = do
  -- Graph: U={1,2}, V={1,2}, edges: 1-1, 1-2, 2-1
  let nU = 2; nV = 2
      adj = buildAdj nU [(1,1),(1,2),(2,1)]
      (size, Matching mu mv) = hopcroftKarp nU nV adj
  assert (size == 2) "test1 size"
  assert (IM.size mu == 2) "test1 matching left size"
  assert (IM.size mv == 2) "test1 matching right size"

test2 :: IO ()
test2 = do
  -- Empty graph
  let nU = 3; nV = 3
      adj = buildAdj nU []
      (size, Matching mu mv) = hopcroftKarp nU nV adj
  assert (size == 0) "test2 size"
  assert (IM.null mu) "test2 mu empty"
  assert (IM.null mv) "test2 mv empty"

test3 :: IO ()
test3 = do
  -- Complete bipartite K3,3
  let nU = 3; nV = 3
      edges = [(u,v) | u <- [1..3], v <- [1..3]]
      adj = buildAdj nU edges
      (size, _) = hopcroftKarp nU nV adj
  assert (size == 3) "test3 size"

test4 :: IO ()
test4 = do
  -- Disconnected components
  let nU = 4; nV = 4
      edges = [(1,1),(2,2),(3,3)]  -- vertex 4 isolated
      adj = buildAdj nU edges
      (size, Matching mu _) = hopcroftKarp nU nV adj
  assert (size == 3) "test4 size"
  assert (IM.notMember 4 mu) "test4 vertex 4 unmatched"

runTests :: IO ()
runTests = do
  test1
  test2
  test3
  test4
  putStrLn "All tests passed."

main :: IO ()
main = runTests
