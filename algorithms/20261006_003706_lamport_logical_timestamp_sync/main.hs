module Main where

import Lamport
import System.Exit (exitFailure)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- Simple assertion helper
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual testName expected actual =
  if expected == actual
    then putStrLn $ "[PASS] " ++ testName
    else do
      putStrLn $ "[FAIL] " ++ testName
      putStrLn $ "  Expected: " ++ show expected
      putStrLn $ "  Actual:   " ++ show actual
      exitFailure

-- Test 1: Node initialization and increment
testIncrement :: IO ()
testIncrement = do
  let node0 = newNode 0
  let node1 = increment node0
  assertEqual "Increment from 0" 1 (getTimestamp node1)

-- Test 2: Sending a message
testSend :: IO ()
testSend = do
  let node0 = newNode 5
  let (node1, msg) = send node0
  assertEqual "Send increments node" 6 (getTimestamp node1)
  assertEqual "Message timestamp" 5 (case msg of Message t -> t)

-- Test 3: Receiving a message
testReceive :: IO ()
testReceive = do
  let node0 = newNode 2
  let msg = Message 5
  let node1 = receive node0 msg
  assertEqual "Receive updates timestamp" 6 (getTimestamp node1)

-- Test 4: Two-way communication
testTwoWay :: IO ()
testTwoWay = do
  let a0 = newNode 0
  let b0 = newNode 0
  -- A sends to B
  let (a1, msgAB) = send a0
  let b1 = receive b0 msgAB
  -- B sends back to A
  let (b2, msgBA) = send b1
  let a2 = receive a1 msgBA
  assertEqual "Final A timestamp" 4 (getTimestamp a2)
  assertEqual "Final B timestamp" 4 (getTimestamp b2)

-- Benchmark: many increments
benchmark :: IO ()
benchmark = do
  let iterations = 1000000
  start <- getCPUTime
  let finalNode = foldl (\n _ -> increment n) (newNode 0) [1..iterations]
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10^12) :: Double
  putStrLn $ printf "Benchmark: %d increments in %.3f seconds" iterations diff

main :: IO ()
main = do
  putStrLn "Running Lamport Logical Timestamp Tests..."
  testIncrement
  testSend
  testReceive
  testTwoWay
  putStrLn "All tests passed."
  benchmark
