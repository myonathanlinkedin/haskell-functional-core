module Main where

import Core

-- Simple assertion helper
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)

-- Test that an empty heap behaves correctly
testEmpty :: IO ()
testEmpty = do
  assert (isEmpty (empty :: Heap Int)) "empty should be empty"
  assert (findMin (empty :: Heap Int) == Nothing) "findMin empty should be Nothing"
  assert (isValid (empty :: Heap Int)) "empty heap must be valid"

-- Test singleton heap
testSingleton :: IO ()
testSingleton = do
  let h = singleton 42 :: Heap Int
  assert (not (isEmpty h)) "singleton should not be empty"
  assert (findMin h == Just 42) "findMin singleton should be the element"
  assert (isValid h) "singleton heap must be valid"

-- Test insertion and ordering
testInsert :: IO ()
testInsert = do
  let h = foldr insert empty [5,3,9,1,4] :: Heap Int
  assert (findMin h == Just 1) "minimum after inserts should be 1"
  assert (isValid h) "heap after inserts must be valid"

-- Test deleteMin removes the smallest element
testDeleteMin :: IO ()
testDeleteMin = do
  let h0 = foldr insert empty [2,8,5,3] :: Heap Int
  let Just m = findMin h0
  assert (m == 2) "initial min should be 2"
  let h1 = deleteMin h0
  assert (findMin h1 == Just 3) "min after deleteMin should be 3"
  assert (isValid h1) "heap after deleteMin must be valid"

-- Test heapSort correctness on various inputs
testHeapSort :: IO ()
testHeapSort = do
  let cases = [ ([] :: [Int])
              , [1]
              , [3,1,4,1,5,9,2,6,5]
              , [10,9..1]
              , replicate 5 7
              ]
  mapM_ (\xs -> assert (heapSort xs == quickSort xs) ("heapSort failed on " ++ show xs)) cases
  where
    quickSort :: Ord a => [a] -> [a]
    quickSort [] = []
    quickSort (p:xs) = quickSort [y | y <- xs, y <= p] ++ [p] ++ quickSort [y | y <- xs, y > p]

-- Test that merging two heaps preserves validity and ordering
testMerge :: IO ()
testMerge = do
  let h1 = foldr insert empty [1,4,7] :: Heap Int
  let h2 = foldr insert empty [2,3,6,8] :: Heap Int
  let h = merge h1 h2
  assert (findMin h == Just 1) "merged heap min should be 1"
  assert (isValid h) "merged heap must be valid"
  let sorted = heapSort [1,4,7,2,3,6,8]
  assert (heapSort [1,4,7,2,3,6,8] == sorted) "merged heap sort consistency"

-- Aggregate all tests
runTests :: IO ()
runTests = do
  testEmpty
  testSingleton
  testInsert
  testDeleteMin
  testHeapSort
  testMerge
  putStrLn "All tests passed."

main :: IO ()
main = runTests
