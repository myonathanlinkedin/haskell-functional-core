module Main where

import Core (RingBuffer, newRingBuffer, push, pop)
import Control.Concurrent (forkIO, threadDelay, MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad (replicateM_, when, void)
import Data.IORef (newIORef, atomicModifyIORef', readIORef)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Exit (exitFailure)

-- Simple assertion helper
assert :: Bool -> String -> IO ()
assert True _  = return ()
assert False msg = putStrLn ("ASSERTION FAILED: " ++ msg) >> exitFailure

-- Basic functional test
testBasic :: IO Bool
testBasic = do
  rb <- newRingBuffer 2
  ok1 <- push rb 1
  ok2 <- push rb 2
  ok3 <- push rb 3          -- should fail (full)
  v1  <- pop rb
  v2  <- pop rb
  v3  <- pop rb
  let pass = and [ ok1
                 , ok2
                 , not ok3
                 , v1 == Just 1
                 , v2 == Just 2
                 , v3 == Nothing
                 ]
  return pass

-- Concurrency stress test
testConcurrency :: IO Bool
testConcurrency = do
  let cap = 1024
      totalOps = 100000
      producers = 4
      consumers = 4
      opsPerProd = totalOps `div` producers

  rb <- newRingBuffer cap
  pushedCount <- newIORef 0
  poppedCount <- newIORef 0
  sumMVar <- newMVar (0 :: Int)

  -- Producer threads
  replicateM_ producers $ \pid -> forkIO $ do
    let start = pid * opsPerProd
        end   = start + opsPerProd
    forM_ [start .. end - 1] $ \i -> do
      success <- push rb i
      when success $ atomicModifyIORef' pushedCount (\c -> (c+1, ()))
      -- simple back‑off if buffer full
      unless success $ threadDelay 1

  -- Consumer threads
  replicateM_ consumers $ \_ -> forkIO $ do
    let loop = do
          mx <- pop rb
          case mx of
            Nothing -> do
              cur <- readIORef poppedCount
              when (cur < totalOps) $ threadDelay 1 >> loop
            Just v  -> do
              atomicModifyIORef' poppedCount (\c -> (c+1, ()))
              modifyMVar_ sumMVar (\s -> return (s + v))
              cur <- readIORef poppedCount
              when (cur < totalOps) loop
    loop

  -- Wait for completion
  let wait = do
        p <- readIORef pushedCount
        c <- readIORef poppedCount
        when (c < totalOps) $ threadDelay 1000 >> wait
  wait

  finalSum <- readMVar sumMVar
  let expectedSum = sum [0 .. totalOps - 1]
  return (finalSum == expectedSum)

-- Benchmark pushing and popping in a single thread
benchmark :: IO ()
benchmark = do
  let cap = 4096
      ops = 500000
  rb <- newRingBuffer cap
  start <- getCurrentTime
  replicateM_ ops $ do
    _ <- push rb ()
    _ <- pop rb
    return ()
  end <- getCurrentTime
  putStrLn $ "Single‑thread benchmark: " ++ show (diffUTCTime end start)

main :: IO ()
main = do
  putStrLn "Running basic functional test..."
  basicPass <- testBasic
  putStrLn $ "Basic test " ++ (if basicPass then "PASS" else "FAIL")
  assert basicPass "Basic test failed"

  putStrLn "Running concurrency stress test..."
  concPass <- testConcurrency
  putStrLn $ "Concurrency test " ++ (if concPass then "PASS" else "FAIL")
  assert concPass "Concurrency test failed"

  putStrLn "Running benchmark..."
  benchmark
  putStrLn "All done."
