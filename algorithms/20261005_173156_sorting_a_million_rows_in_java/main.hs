module Main where

import Types
import Engine
import System.Exit (exitFailure)
import Data.List (sort)

-- Simple assertion helper
assert :: Bool -> String -> IO ()
assert True _   = return ()
assert False msg = putStrLn ("Assertion failed: " ++ msg) >> exitFailure

-- Edge‑case tests
testEmpty :: IO ()
testEmpty = do
  result <- processRows 0
  assert (sortedRows result == []) "Empty list should remain empty"
  assert (timeGen result >= 0) "Generation time must be non‑negative"
  assert (timeSort result >= 0) "Sorting time must be non‑negative"

testSingle :: IO ()
testSingle = do
  let rows = [42]
  let result = SortResult rows 0 0
  assert (sortedRows result == [42]) "Single element list should stay unchanged"

testSorted :: IO ()
testSorted = do
  let rows = [1..1000]
  let result = SortResult rows 0 0
  assert (sortedRows result == rows) "Already sorted list should stay unchanged"

testReverse :: IO ()
testReverse = do
  let rows = reverse [1..1000]
  let expected = [1..1000]
  let result = SortResult (sort rows) 0 0
  assert (sortedRows result == expected) "Reverse sorted list should become sorted"

-- Demonstration with one million rows
demo :: IO ()
demo = do
  putStrLn "Generating and sorting 1,000,000 rows..."
  result <- processRows 1000000
  putStrLn $ "Generation time (s): " ++ show (timeGen result)
  putStrLn $ "Sorting time (s):    " ++ show (timeSort result)
  putStrLn $ "First 10 sorted rows: " ++ show (take 10 (sortedRows result))

main :: IO ()
main = do
  testEmpty
  testSingle
  testSorted
  testReverse
  demo
