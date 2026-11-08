module Main where

import Core
import Data.Array
import Data.Bits
import Data.List
import qualified Data.Map.Strict as Map

-- Build a path graph and its tree decomposition
buildPathGraph :: Int -> Graph
buildPathGraph n = buildGraph n edges
  where
    edges = [(i,i+1) | i <- [0..n-2]]

buildPathDecomposition :: Int -> TreeNode
buildPathDecomposition n
  | n <= 1    = TreeNode [] []
  | otherwise = foldr1 link nodes
  where
    nodes = [ TreeNode [i,i+1] [] | i <- [0..n-2] ]
    link a b = TreeNode (bag a) [b]

-- Brute force count of independent sets
bruteCountIndependentSets :: Graph -> Integer
bruteCountIndependentSets g = fromIntegral $ length [ mask | mask <- [0 .. (1 `shiftL` n) - 1], isIndependent mask ]
  where
    n = snd (bounds g) + 1
    edges = [ (u,v) | u <- [0..n-1], v <- g ! u, u < v ]
    isIndependent mask = all (\(u,v) -> not (testBit mask u && testBit mask v)) edges

-- Simple assertion helper
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual name expected actual =
  if expected == actual
    then putStrLn $ "✓ " ++ name
    else error $ "✗ " ++ name ++ ": expected " ++ show expected ++ ", got " ++ show actual

-- Test cases
testEmptyGraph :: IO ()
testEmptyGraph = do
  let g = buildGraph 0 []
  let root = TreeNode [] []
  let alg = countIndependentSets g root
  let brute = bruteCountIndependentSets g
  assertEqual "Empty graph" 1 alg
  assertEqual "Empty graph brute" 1 brute

testSingleVertex :: IO ()
testSingleVertex = do
  let g = buildGraph 1 []
  let root = TreeNode [0] []
  let alg = countIndependentSets g root
  let brute = bruteCountIndependentSets g
  assertEqual "Single vertex" 2 alg
  assertEqual "Single vertex brute" 2 brute

testTwoNoEdge :: IO ()
testTwoNoEdge = do
  let g = buildGraph 2 []
  let root = TreeNode [0,1] []
  let alg = countIndependentSets g root
  let brute = bruteCountIndependentSets g
  assertEqual "Two vertices no edge" 4 alg
  assertEqual "Two vertices no edge brute" 4 brute

testTwoEdge :: IO ()
testTwoEdge = do
  let g = buildGraph 2 [(0,1)]
  let root = TreeNode [0,1] []
  let alg = countIndependentSets g root
  let brute = bruteCountIndependentSets g
  assertEqual "Two vertices with edge" 3 alg
  assertEqual "Two vertices with edge brute" 3 brute

testTriangle :: IO ()
testTriangle = do
  let g = buildGraph 3 [(0,1),(1,2),(0,2)]
  let root = TreeNode [0,1,2] []
  let alg = countIndependentSets g root
  let brute = bruteCountIndependentSets g
  assertEqual "Triangle" 4 alg
  assertEqual "Triangle brute" 4 brute

testPath3 :: IO ()
testPath3 = do
  let g = buildPathGraph 3
  let root = buildPathDecomposition 3
  let alg = countIndependentSets g root
  let brute = bruteCountIndependentSets g
  assertEqual "Path 3" 5 alg
  assertEqual "Path 3 brute" 5 brute

testPath4 :: IO ()
testPath4 = do
  let g = buildPathGraph 4
  let root = buildPathDecomposition 4
  let alg = countIndependentSets g root
  let brute = bruteCountIndependentSets g
  assertEqual "Path 4" 8 alg
  assertEqual "Path 4 brute" 8 brute

main :: IO ()
main = do
  testEmptyGraph
  testSingleVertex
  testTwoNoEdge
  testTwoEdge
  testTriangle
  testPath3
  testPath4
  putStrLn "All tests passed."
