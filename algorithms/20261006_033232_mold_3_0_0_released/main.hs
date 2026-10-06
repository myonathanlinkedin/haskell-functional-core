module Main where

import Types
import Engine
import System.Exit (exitFailure)

-- Simple assertion helper.
assert :: Bool -> String -> IO ()
assert True  _   = pure ()
assert False msg = putStrLn ("ASSERTION FAILED: " ++ msg) >> exitFailure

-- Test suite for the Mold data structure.
runTests :: IO ()
runTests = do
  -- Start with an empty mold.
  let m0 = emptyMold :: Mold String Int

  -- Insert some values.
  let Success m1 = insertMold "apple" 3 m0
  let Success m2 = insertMold "banana" 5 m1

  -- Lookup existing key.
  vApple <- unwrapResult $ lookupMold "apple" m2
  assert (vApple == 3) "apple should be 3"

  -- Lookup missing key should fail.
  case lookupMold "cherry" m2 of
    Failure _ -> pure ()
    Success _ -> assert False "lookup of missing key should fail"

  -- Delete a key.
  let Success m3 = deleteMold "banana" m2
  assert (listKeys m3 == ["apple"]) "Only apple should remain after deletion"

  -- Deleting non‑existent key should fail.
  case deleteMold "banana" m3 of
    Failure _ -> pure ()
    Success _ -> assert False "deleting missing key should fail"

  -- Take a snapshot.
  let Success (m4, sid1) = snapshotMold m3
  assert (sid1 == SnapshotId 0) "First snapshot id should be 0"

  -- Mutate after snapshot.
  let Success m5 = insertMold "date" 7 m4
  assert (listKeys m5 == ["apple","date"]) "date should be added"

  -- Take second snapshot.
  let Success (m6, sid2) = snapshotMold m5
  assert (sid2 == SnapshotId 1) "Second snapshot id should be 1"

  -- Revert to first snapshot.
  let Success m7 = revertMold sid1 m6
  assert (listKeys m7 == ["apple"]) "Revert should restore state to only apple"

  -- Revert to unknown snapshot should fail.
  case revertMold (SnapshotId 999) m7 of
    Failure _ -> pure ()
    Success _ -> assert False "reverting to unknown snapshot should fail"

  -- Verify snapshot list order.
  let snaps = listSnapshots m6
  assert (snaps == [SnapshotId 0, SnapshotId 1]) "Snapshot list order mismatch"

  putStrLn "All tests passed."

main :: IO ()
main = runTests
