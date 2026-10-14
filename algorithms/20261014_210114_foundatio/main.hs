module Main where

import Heap
import System.Exit (exitFailure)

-- Simple assertion helper.
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = putStrLn ("Assertion failed: " ++ msg) >> exitFailure

-- Test that the empty heap behaves correctly.
testEmpty :: IO ()
testEmpty = do
    let h = empty :: Heap Int
    assert (isValid h) "empty heap invalid"
    assert (findMin h == Nothing) "empty findMin not Nothing"
    assert (toList h == []) "empty toList not []"

-- Test insertion and ordering.
testInsert :: IO ()
testInsert = do
    let h1 = insert 5 (empty :: Heap Int)
    assert (isValid h1) "single insert invalid"
    assert (findMin h1 == Just 5) "single insert findMin"
    let h2 = insert 3 (insert 7 h1)
    assert (isValid h2) "multiple insert invalid"
    assert (findMin h2 == Just 3) "multiple insert findMin"
    assert (toList h2 == [3,5,7]) "toList after inserts"

-- Test merging two heaps.
testMerge :: IO ()
testMerge = do
    let h1 = fromList [1,4,6] :: Heap Int
    let h2 = fromList [2,3,5] :: Heap Int
    let h  = merge h1 h2
    assert (isValid h) "merged heap invalid"
    assert (toList h == [1..6]) "merged heap not sorted"

-- Test building a heap from a list.
testFromList :: IO ()
testFromList = do
    let xs = [10,9..1]
    let h = fromList xs :: Heap Int
    assert (isValid h) "fromList heap invalid"
    assert (toList h == [1..10]) "fromList toList not sorted"

-- Test deleteMin operation.
testDeleteMin :: IO ()
testDeleteMin = do
    let h  = fromList [1,2,3,4,5] :: Heap Int
    let h' = deleteMin h
    assert (isValid h') "deleteMin heap invalid"
    assert (toList h' == [2,3,4,5]) "deleteMin result incorrect"

-- Entry point running all tests.
main :: IO ()
main = do
    testEmpty
    testInsert
    testMerge
    testFromList
    testDeleteMin
    putStrLn "All tests passed."
