module Main where

import Core
import System.Exit (exitFailure)

assert :: Bool -> String -> IO ()
assert True _   = return ()
assert False m = putStrLn ("Assertion failed: " ++ m) >> exitFailure

testInsert :: IO ()
testInsert = do
  let vals = [10,20,30,40,50,25]
      avl  = fromList vals
  assert (isBalanced avl) "AVL not balanced after inserts"
  mapM_ (\x -> assert (member x avl) ("Missing member " ++ show x)) vals

testDelete :: IO ()
testDelete = do
  let vals = [10,20,30,40,50,25]
      avl0 = fromList vals
      avl1 = delete 20 avl0
  assert (isBalanced avl1) "AVL not balanced after delete"
  assert (not (member 20 avl1)) "Deleted element still present"
  mapM_ (\x -> if x /= 20
               then assert (member x avl1) ("Missing after delete " ++ show x)
               else return ()) vals

testOrder :: IO ()
testOrder = do
  let vals = [5,3,8,1,4,7,9]
      avl  = fromList vals
  assert (toList avl == [1,3,4,5,7,8,9]) "Inorder traversal incorrect"

main :: IO ()
main = do
  testInsert
  testDelete
  testOrder
  putStrLn "All tests passed."
