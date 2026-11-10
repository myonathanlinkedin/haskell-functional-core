module Main where

import Core

-- Simple assertion helper that reports success or aborts on failure.
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual msg expected actual =
  if expected == actual
    then putStrLn $ "PASS: " ++ msg
    else error $ "FAIL: " ++ msg ++ "\nExpected: " ++ show expected ++ "\nActual:   " ++ show actual

-- Basic functional tests covering versioning, retrieval, and pruning.
testBasic :: IO ()
testBasic = do
  let s0 = emptyStore :: Store String Int
      s1 = set "a" 1 s0
      s2 = set "b" 2 s1
      s3 = set "a" 3 s2
  assertEqual "latestVersion" 3 (latestVersion s3)
  assertEqual "get a (latest)" (Just 3) (get "a" s3)
  assertEqual "get b (latest)" (Just 2) (get "b" s3)
  assertEqual "getAt a version1" (Just 1) (getAt "a" 1 s3)
  assertEqual "getAt a version2" (Just 1) (getAt "a" 2 s3)
  assertEqual "getAt a version3" (Just 3) (getAt "a" 3 s3)
  let s4 = pruneOlderThan 2 s3
  assertEqual "prune retains version2" (Just 1) (getAt "a" 2 s4)
  assertEqual "prune discards version1" Nothing (getAt "a" 1 s4)

-- Edge‑case tests for empty store and single‑element operations.
testEdge :: IO ()
testEdge = do
  let s0 = emptyStore :: Store Int String
  assertEqual "empty get" Nothing (get 42 s0)
  let s1 = set 42 "answer" s0
  assertEqual "single set get" (Just "answer") (get 42 s1)
  assertEqual "single set getAt version0" Nothing (getAt 42 0 s1)
  assertEqual "single set getAt version1" (Just "answer") (getAt 42 1 s1)

main :: IO ()
main = do
  putStrLn "Running tests..."
  testBasic
  testEdge
  putStrLn "All tests passed."
