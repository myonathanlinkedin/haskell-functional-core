module Main where

import Core

-- Simple assertion helper.
assert :: Bool -> String -> IO ()
assert True _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)

-- Test that insertions and deletions keep the list ordered and update versions.
testInsertDelete :: IO ()
testInsertDelete = do
  let m0 = fromList [(1,'a'),(3,'c'),(5,'e')]
  let m1 = insert 2 'b' m0
  let m2 = insert 4 'd' m1
  let m3 = delete 3 m2
  assert (toList m3 == [(1,'a'),(2,'b'),(4,'d'),(5,'e')])
         "Insert/Delete order preservation"

-- Test a successful optimistic read followed by validation (no intervening writes).
testOptimisticSuccess :: IO ()
testOptimisticSuccess = do
  let m = fromList [(1,'a'),(2,'b'),(3,'c')]
  case optimisticRead 2 m of
    Just (v, vs) -> do
      assert (v == 'b') "Read correct value"
      assert (validateRead 2 m vs) "Validation succeeds when unchanged"
    Nothing -> error "Optimistic read unexpectedly failed"

-- Test that a modification invalidates a previously observed version list.
testOptimisticFailure :: IO ()
testOptimisticFailure = do
  let m0 = fromList [(1,'a'),(2,'b'),(3,'c')]
  let Just (_, vs) = optimisticRead 2 m0
  let m1 = insert 0 'z' m0   -- changes the head node's version
  assert (not (validateRead 2 m1 vs))
         "Validation fails after concurrent modification"

-- Reading a non‑existent key should yield Nothing.
testReadNonexistent :: IO ()
testReadNonexistent = do
  let m = fromList [(1,'a'),(3,'c')]
  assert (optimisticRead 2 m == Nothing) "Non‑existent key returns Nothing"

main :: IO ()
main = do
  testInsertDelete
  testOptimisticSuccess
  testOptimisticFailure
  testReadNonexistent
  putStrLn "All tests passed."
