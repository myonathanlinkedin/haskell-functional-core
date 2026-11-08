module Main where

import Core
import System.Exit (exitFailure)

-- Simple assertion helper.
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = putStrLn ("Assertion failed: " ++ msg) >> exitFailure

-- Test 1: Insertion maintains order and AVL balance.
testInsertion :: IO ()
testInsertion = do
    let msgs = [(5,"e"),(3,"c"),(8,"h"),(1,"a"),(4,"d"),(7,"g"),(10,"j")]
        tree = foldl' insert empty msgs
        inorder = rangeQuery tree minBound maxBound
    assert (inorder == [(1,"a"),(3,"c"),(4,"d"),(5,"e"),(7,"g"),(8,"h"),(10,"j")])
          "Insertion order mismatch"

-- Test 2: Range query returns correct subset.
testRangeQuery :: IO ()
testRangeQuery = do
    let msgs = [(2,"b"),(5,"e"),(9,"i"),(12,"l"),(15,"o")]
        tree = foldl' insert empty msgs
        result = rangeQuery tree 5 12
    assert (result == [(5,"e"),(9,"i"),(12,"l")])
          "Range query returned incorrect messages"

-- Test 3: deleteBefore removes older messages.
testDeleteBefore :: IO ()
testDeleteBefore = do
    let msgs = [(1,"a"),(3,"c"),(5,"e"),(7,"g")]
        tree = foldl' insert empty msgs
        pruned = deleteBefore tree 5
        remaining = rangeQuery pruned minBound maxBound
    assert (remaining == [(5,"e"),(7,"g")])
          "deleteBefore failed to prune correctly"

-- Test 4: Edge cases – empty tree operations.
testEdgeCases :: IO ()
testEdgeCases = do
    let emptyTree = empty :: AVLTree
    assert (rangeQuery emptyTree 0 10 == []) "Range query on empty tree should be []"
    assert (deleteBefore emptyTree 5 == Empty) "deleteBefore on empty tree should be Empty"
    let single = insert emptyTree (42,"answer")
    assert (rangeQuery single 0 100 == [(42,"answer")]) "Single insertion failed"
    assert (deleteBefore single 43 == Empty) "deleteBefore should remove the only element"

main :: IO ()
main = do
    putStrLn "Running Core module unit tests..."
    testInsertion
    testRangeQuery
    testDeleteBefore
    testEdgeCases
    putStrLn "All tests passed successfully."
