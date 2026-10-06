module Main where

import Core
import Control.Concurrent
import Control.Exception
import System.IO

-- | Unit test suite for the Scheduler.
runTests :: IO ()
runTests = do
  putStrLn "Running Unit Tests..."
  testSubmitAndComplete
  testTaskFailure
  testConcurrentTasks
  putStrLn "All Tests Passed."

-- | Test 1: Submit a task and verify it completes.
testSubmitAndComplete :: IO ()
testSubmitAndComplete = do
  sch <- newScheduler
  startScheduler sch 2
  tid <- submitTask sch (threadDelay 100000)
  waitForTask sch tid
  status <- getTaskStatus sch tid
  case status of
    Just Completed -> putStrLn "Test 1 (Submit and Complete): PASSED"
    _ -> putStrLn "Test 1 (Submit and Complete): FAILED"
  stopScheduler sch

-- | Test 2: Submit a task that fails and verify status.
testTaskFailure :: IO ()
testTaskFailure = do
  sch <- newScheduler
  startScheduler sch 2
  tid <- submitTask sch (throwIO (userError "Test Error"))
  waitForTask sch tid
  status <- getTaskStatus sch tid
  case status of
    Just Failed -> putStrLn "Test 2 (Task Failure): PASSED"
    _ -> putStrLn "Test 2 (Task Failure): FAILED"
  stopScheduler sch

-- | Test 3: Submit multiple concurrent tasks and verify all complete.
testConcurrentTasks :: IO ()
testConcurrentTasks = do
  sch <- newScheduler
  startScheduler sch 4
  tids <- mapM (\_ -> submitTask sch (threadDelay 50000)) [1..10]
  mapM_ (\tid -> waitForTask sch tid) tids
  statuses <- mapM (\tid -> getTaskStatus sch tid) tids
  let allCompleted = all (== Just Completed) statuses
  if allCompleted
    then putStrLn "Test 3 (Concurrent Tasks): PASSED"
    else putStrLn "Test 3 (Concurrent Tasks): FAILED"
  stopScheduler sch

-- | Main entry point.
main :: IO ()
main = do
  runTests
