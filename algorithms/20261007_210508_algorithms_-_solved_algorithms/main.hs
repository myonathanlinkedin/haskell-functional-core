module Main where

import Types (BST(..))
import Engine (insert, delete, find, inorder)

assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False msg = error ("Assertion failed: " ++ msg)

testInsert :: IO ()
testInsert = do
    let t0 = Empty :: BST Int
        t1 = insert 5 t0
        t2 = insert 3 t1
        t3 = insert 7 t2
        t4 = insert 4 t3
    assert (inorder t4 == [3,4,5,7]) "Insert inorder mismatch"
    assert (find 5 t4) "Find existing element failed"
    assert (not (find 6 t4)) "Find non-existing element failed"

testDeleteLeaf :: IO ()
testDeleteLeaf = do
    let t = foldr insert Empty [5,3,7,2,4]
        t' = delete 2 t
    assert (inorder t' == [3,4,5,7]) "Delete leaf failed"

testDeleteOneChild :: IO ()
testDeleteOneChild = do
    let t = foldr insert Empty [5,3,7,2,4,6]
        t' = delete 7 t  -- 7 has left child 6
    assert (inorder t' == [2,3,4,5,6]) "Delete node with one child failed"

testDeleteTwoChildren :: IO ()
testDeleteTwoChildren = do
    let t = foldr insert Empty [5,3,7,2,4,6,8]
        t' = delete 5 t  -- root with two children
    assert (inorder t' == [2,3,4,6,7,8]) "Delete node with two children failed"

testDeleteNonexistent :: IO ()
testDeleteNonexistent = do
    let t = foldr insert Empty [1,2,3]
        t' = delete 99 t
    assert (inorder t' == [1,2,3]) "Delete nonexistent altered tree"

main :: IO ()
main = do
    testInsert
    testDeleteLeaf
    testDeleteOneChild
    testDeleteTwoChildren
    testDeleteNonexistent
    putStrLn "All tests passed."
