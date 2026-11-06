module Main where

import AVL
import System.Exit (exitFailure)

-- Simple assertion helper
assert :: Bool -> String -> IO ()
assert True _  = return ()
assert False msg = putStrLn ("Assertion failed: " ++ msg) >> exitFailure

-- Test insertion and lookup
testInsertLookup :: IO ()
testInsertLookup = do
    let t0 = empty :: AVL Int String
        t1 = insert 5 "five" t0
        t2 = insert 3 "three" t1
        t3 = insert 7 "seven" t2
        t4 = insert 4 "four" t3
    assert (lookup 5 t4 == Just "five") "lookup 5"
    assert (lookup 3 t4 == Just "three") "lookup 3"
    assert (lookup 7 t4 == Just "seven") "lookup 7"
    assert (lookup 4 t4 == Just "four") "lookup 4"
    assert (lookup 2 t4 == Nothing) "lookup missing"
    assert (isBalanced t4) "tree balanced after inserts"

-- Test deletion cases
testDelete :: IO ()
testDelete = do
    let elems = [(10,"a"),(20,"b"),(30,"c"),(40,"d"),(50,"e")]
        t = fromList elems :: AVL Int String
    assert (isBalanced t) "initial balanced"
    let t1 = delete 20 t
    assert (lookup 20 t1 == Nothing) "deleted 20"
    assert (isBalanced t1) "balanced after deleting node with one child"
    let t2 = delete 30 t1
    assert (lookup 30 t2 == Nothing) "deleted 30"
    assert (isBalanced t2) "balanced after deleting node with two children"
    let t3 = delete 10 t2
    assert (lookup 10 t3 == Nothing) "deleted 10 (root)"
    assert (isBalanced t3) "balanced after deleting root"
    let t4 = delete 100 t3  -- non‑existent key
    assert (t4 == t3) "delete non‑existent leaves tree unchanged"

-- Test fromList / toList round‑trip and ordering
testFromToList :: IO ()
testFromToList = do
    let pairs = [(5,"e"),(1,"a"),(3,"c"),(2,"b"),(4,"d")]
        t = fromList pairs :: AVL Int String
    assert (toList t == [(1,"a"),(2,"b"),(3,"c"),(4,"d"),(5,"e")]) "toList sorted"
    assert (isBalanced t) "balanced after fromList"

-- Run all tests
main :: IO ()
main = do
    testInsertLookup
    testDelete
    testFromToList
    putStrLn "All AVL tree tests passed."
