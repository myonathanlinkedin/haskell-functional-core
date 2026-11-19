module Main where

import Core
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import System.Exit (exitFailure, exitSuccess)

-- Simple assertion helper.
assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False msg = putStrLn ("Assertion failed: " ++ msg) >> exitFailure

-- Test case 1: all participants vote Commit -> coordinator decides Commit.
testAllCommit :: IO ()
testAllCommit = do
    let participants = [1,2,3]
        votes = Map.fromList [(1, Commit), (2, Commit), (3, Commit)]
        (cFinal, pFinal) = runTwoPhaseCommit participants votes
    -- Coordinator must be Completed with CommitDecision.
    case cFinal of
        CCompleted CommitDecision -> return ()
        _ -> assert False "Coordinator did not complete with CommitDecision"
    -- All participants must be in PCommitted.
    mapM_ (\pid ->
        case Map.lookup pid pFinal of
            Just PCommitted -> return ()
            _ -> assert False ("Participant " ++ show pid ++ " not committed")
        ) participants

-- Test case 2: one participant votes Abort -> coordinator decides Abort.
testOneAbort :: IO ()
testOneAbort = do
    let participants = [1,2,3]
        votes = Map.fromList [(1, Commit), (2, Abort), (3, Commit)]
        (cFinal, pFinal) = runTwoPhaseCommit participants votes
    case cFinal of
        CCompleted AbortDecision -> return ()
        _ -> assert False "Coordinator did not complete with AbortDecision"
    -- All participants must be in PAborted.
    mapM_ (\pid ->
        case Map.lookup pid pFinal of
            Just PAborted -> return ()
            _ -> assert False ("Participant " ++ show pid ++ " not aborted")
        ) participants

-- Test case 3: empty participant list -> coordinator should immediately complete.
testEmpty :: IO ()
testEmpty = do
    let participants = []
        votes = Map.empty
        (cFinal, pFinal) = runTwoPhaseCommit participants votes
    case cFinal of
        CCompleted CommitDecision -> return ()
        _ -> assert False "Empty coordinator did not default to CommitDecision"
    assert (Map.null pFinal) "Participant map should be empty"

main :: IO ()
main = do
    testAllCommit
    testOneAbort
    testEmpty
    putStrLn "All tests passed."
    exitSuccess
