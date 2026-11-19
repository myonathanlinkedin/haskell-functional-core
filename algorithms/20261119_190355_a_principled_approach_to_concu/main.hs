module Main where

import Types
import Engine
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import System.Exit (exitFailure, exitSuccess)

-- | Test helper: assert a condition, print result.
assert :: String -> Bool -> IO ()
assert msg cond =
  if cond
    then putStrLn ("  PASS: " ++ msg)
    else do
      putStrLn ("  FAIL: " ++ msg)
      exitFailure

-- | Test 1: Simple send-recv choreography.
testSimpleSendRecv :: IO ()
testSimpleSendRecv = do
  putStrLn "Test 1: Simple send-recv"
  let parts = S.fromList [P1, P2]
      choreo = [ Send P1 P2 (IntMsg 42), Recv P2 P1 (IntMsg 42) ]
      result = runChoreo parts choreo
  assert "Choreography completes successfully" (success result)
  assert "No errors in execution" (null (errors (finalState result)))
  assert "Step index reached end" (stepIndex (finalState result) == 2)
  assert "P1 log has 1 step" (length (log (M.lookup P1 (localStates (finalState result)) & fmap (fromMaybe initialLocalState))) == 1)
  assert "P2 log has 1 step" (length (log (M.lookup P2 (localStates (finalState result)) & fmap (fromMaybe initialLocalState))) == 1)
  assert "P1 pending sends empty" (M.null (pendingSends (M.lookup P1 (localStates (finalState result)) & fmap (fromMaybe initialLocalState))))
  assert "P2 pending recvs empty" (M.null (pendingRecvs (M.lookup P2 (localStates (finalState result)) & fmap (fromMaybe initialLocalState))))

-- | Test 2: Multiple participants.
testMultipleParticipants :: IO ()
testMultipleParticipants = do
  putStrLn "Test 2: Multiple participants"
  let parts = S.fromList [P1, P2, P3]
      choreo = [ Send P1 P2 (BoolMsg True), Recv P2 P1 (BoolMsg True)
               , Send P2 P3 (IntMsg 7), Recv P3 P2 (IntMsg 7)
               , Send P3 P1 (UnitMsg), Recv P1 P3 (UnitMsg) ]
      result = runChoreo parts choreo
  assert "Choreography completes successfully" (success result)
  assert "No errors" (null (errors (finalState result)))
  assert "Step index = 6" (stepIndex (finalState result) == 6)
  assert "P1 log has 2 steps" (length (log (M.lookup P1 (localStates (finalState result)) & fmap (fromMaybe initialLocalState))) == 2)
  assert "P2 log has 2 steps" (length (log (M.lookup P2 (localStates (finalState result)) & fmap (fromMaybe initialLocalState))) == 2)
  assert "P3 log has 2 steps" (length (log (M.lookup P3 (localStates (finalState result)) & fmap (fromMaybe initialLocalState))) == 2)

-- | Test 3: Mismatched message causes error.
testMismatchedMessage :: IO ()
testMismatchedMessage = do
  putStrLn "Test 3: Mismatched message"
  let parts = S.fromList [P1, P2]
      choreo = [ Send P1 P2 (IntMsg 42), Recv P2 P1 (IntMsg 99) ]
      result = runChoreo parts choreo
  assert "Choreography fails" (not (success result))
  assert "Error message present" (not (null (errors (finalState result))))

-- | Test 4: Unknown participant causes error.
testUnknownParticipant :: IO ()
testUnknownParticipant = do
  putStrLn "Test 4: Unknown participant"
  let parts = S.fromList [P1, P2]
      choreo = [ Send P1 P3 (IntMsg 42) ]
      result = runChoreo parts choreo
  assert "Choreography fails" (not (success result))
  assert "Error message present" (not (null (errors (finalState result))))

-- | Test 5: Well-formedness check.
testWellFormed :: IO ()
testWellFormed = do
  putStrLn "Test 5: Well-formedness check"
  let choreo1 = [ Send P1 P2 (IntMsg 1), Recv P2 P1 (IntMsg 1) ]
      choreo2 = [ Send P1 P2 (IntMsg 1) ]
  assert "Well-formed choreography passes" (checkWellFormed choreo1)
  assert "Incomplete choreography fails" (not (checkWellFormed choreo2))

-- | Test 6: Nondeterministic choice.
testNondeterministicChoice :: IO ()
testNondeterministicChoice = do
  putStrLn "Test 6: Nondeterministic choice"
  let parts = S.fromList [P1, P2]
      branch1 = [ Send P1 P2 (IntMsg 1), Recv P2 P1 (IntMsg 1) ]
      branch2 = [ Send P1 P2 (IntMsg 2), Recv P2 P1 (IntMsg 2) ]
      choice = Choice [branch1, branch2] Nothing
      results = executeChoice parts choice
  assert "Two branches executed" (length results == 2)
  assert "Both branches succeed" (all success results)

-- | Test 7: Asynchronous interleaving.
testAsynchronousInterleaving :: IO ()
testAsynchronousInterleaving = do
  putStrLn "Test 7: Asynchronous interleaving"
  let parts = S.fromList [P1, P2, P3]
      a = [ Send P1 P2 (IntMsg 1), Recv P2 P1 (IntMsg 1) ]
      b = [ Send P2 P3 (IntMsg 2), Recv P3 P2 (IntMsg 2) ]
      results = executeAsync parts a b
  assert "Multiple interleavings produced" (length results > 1)
  assert "All interleavings succeed" (all success results)

-- | Test 8: Empty choreography.
testEmptyChoreography :: IO ()
testEmptyChoreography = do
  putStrLn "Test 8: Empty choreography"
  let parts = S.fromList [P1, P2]
      result = runChoreo parts []
  assert "Empty choreography succeeds" (success result)
  assert "Step index is 0" (stepIndex (finalState result) == 0)

-- | Test 9: Choreography tree flattening.
testChoreoTreeFlattening :: IO ()
testChoreoTreeFlattening = do
  putStrLn "Test 9: Choreography tree flattening"
  let tree = CSeq (CStep (Send P1 P2 (IntMsg 1)) (CStep (Recv P2 P1 (IntMsg 1)) CLeaf))
                  (CStep (Send P2 P1 (BoolMsg True)) (CStep (Recv P1 P2 (BoolMsg True)) CLeaf))
      flat = flattenChoreo tree
  assert "Flattened choreography has 4 steps" (length flat == 4)
  assert "First step is Send P1 P2" (flat !! 0 == Send P1 P2 (IntMsg 1))
  assert "Last step is Recv P1 P2" (flat !! 3 == Recv P1 P2 (BoolMsg True))

-- | Test 10: Verify completion after successful execution.
testVerifyCompletion :: IO ()
testVerifyCompletion = do
  putStrLn "Test 10: Verify completion"
  let parts = S.fromList [P1, P2]
      choreo = [ Send P1 P2 (IntMsg 42), Recv P2 P1 (IntMsg 42) ]
      result = runChoreo parts choreo
  assert "Completion verified" (verifyCompletion (finalState result))

-- | Test 11: Complex choreography with multiple messages.
testComplexChoreography :: IO ()
testComplexChoreography = do
  putStrLn "Test 11: Complex choreography"
  let parts = S.fromList [P1, P2, P3, P4]
      choreo = [ Send P1 P2 (IntMsg 1), Recv P2 P1 (IntMsg 1)
               , Send P2 P3 (IntMsg 2), Recv P3 P2 (IntMsg 2)
               , Send P3 P4 (IntMsg 3), Recv P4 P3 (IntMsg 3)
               , Send P4 P1 (IntMsg 4), Recv P1 P4 (IntMsg 4)
               , Send P1 P3 (BoolMsg True), Recv P3 P1 (BoolMsg True)
               , Send P2 P4 (BoolMsg False), Recv P4 P2 (BoolMsg False) ]
      result = runChoreo parts choreo
  assert "Complex choreography succeeds" (success result)
  assert "Step index = 12" (stepIndex (finalState result) == 12)
  assert "Completion verified" (verifyCompletion (finalState result))

-- | Test 12: Duplicate messages.
testDuplicateMessages :: IO ()
testDuplicateMessages = do
  putStrLn "Test 12: Duplicate messages"
  let parts = S.fromList [P1, P2]
      choreo = [ Send P1 P2 (IntMsg 1), Send P1 P2 (IntMsg 1)
               , Recv P2 P1 (IntMsg 1), Recv P2 P1 (IntMsg 1) ]
      result = runChoreo parts choreo
  assert "Duplicate messages handled" (success result)
  assert "Step index = 4" (stepIndex (finalState result) == 4)

-- | Test 13: Interleaving with empty choreography.
testInterleavingEmpty :: IO ()
testInterleavingEmpty = do
  putStrLn "Test 13: Interleaving with empty choreography"
  let parts = S.fromList [P1, P2]
      a = [ Send P1 P2 (IntMsg 1), Recv P2 P1 (IntMsg 1) ]
      results = executeAsync parts a []
  assert "One interleaving produced" (length results == 1)
  assert "Interleaving succeeds" (success (head results))

-- | Test 14: Choice with single branch.
testChoiceSingleBranch :: IO ()
testChoiceSingleBranch = do
  putStrLn "Test 14: Choice with single branch"
  let parts = S.fromList [P1, P2]
      branch = [ Send P1 P2 (IntMsg 1), Recv P2 P1 (IntMsg 1) ]
      choice = Choice [branch] Nothing
      results = executeChoice parts choice
  assert "One result produced" (length results == 1)
  assert "Result succeeds" (success (head results))

-- | Test 15: Choreography with all message types.
testAllMessageTypes :: IO ()
testAllMessageTypes = do
  putStrLn "Test 15: All message types"
  let parts = S.fromList [P1, P2]
      choreo = [ Send P1 P2 (IntMsg 42), Recv P2 P1 (IntMsg 42)
               , Send P1 P2 (BoolMsg True), Recv P2 P1 (BoolMsg True)
               , Send P1 P2 (UnitMsg), Recv P2 P1 (UnitMsg) ]
      result = runChoreo parts choreo
  assert "All message types handled" (success result)
  assert "Step index = 6" (stepIndex (finalState result) == 6)

-- | Main entry point: run all tests.
main :: IO ()
main = do
  putStrLn "=== Choreography Engine: Unit Tests ==="
  testSimpleSendRecv
  testMultipleParticipants
  testMismatchedMessage
  testUnknownParticipant
  testWellFormed
  testNondeterministicChoice
  testAsynchronousInterleaving
  testEmptyChoreography
  testChoreoTreeFlattening
  testVerifyCompletion
  testComplexChoreography
  testDuplicateMessages
  testInterleavingEmpty
  testChoiceSingleBranch
  testAllMessageTypes
  putStrLn "=== All tests passed ==="
  exitSuccess
