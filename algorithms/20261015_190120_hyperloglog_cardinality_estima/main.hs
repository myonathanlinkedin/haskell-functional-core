module Main where

import Core
import Data.Word
import System.Exit (exitFailure)
import Text.Printf (printf)

-- Simple assertion helper
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = putStrLn ("ASSERTION FAILED: " ++ msg) >> exitFailure

-- Test 1: small cardinalities, check linear counting path
testSmall :: IO ()
testSmall = do
  let h0 = newHLL 4          -- 16 registers
      elems = map fromIntegral [1..10] :: [Word64]
      h = foldr hllAdd h0 elems
      est = hllCount h
  assert (abs (est - 10) < 5) "Small cardinality estimate too far"
  putStrLn $ "testSmall passed (estimate = " ++ show (round est) ++ ")"

-- Test 2: medium cardinality, relative error < 10%
testMedium :: IO ()
testMedium = do
  let p = 10                 -- 1024 registers
      n = 50000
      h0 = newHLL p
      elems = map splitmix64 [1..fromIntegral n] :: [Word64]
      h = foldr hllAdd h0 elems
      est = hllCount h
      err = abs (est - fromIntegral n) / fromIntegral n
  assert (err < 0.1) $ "Medium cardinality relative error too high: " ++ printf "%.2f%%" (err*100)
  putStrLn $ "testMedium passed (estimate = " ++ show (round est) ++ ", error = " ++ printf "%.2f%%" (err*100) ++ ")"

-- Test 3: duplicate elements should not inflate count
testDuplicates :: IO ()
testDuplicates = do
  let h0 = newHLL 8
      elems = replicate 1000 (splitmix64 42)  -- same element many times
      h = foldr hllAdd h0 elems
      est = hllCount h
  assert (est < 5) "Duplicate handling failed"
  putStrLn $ "testDuplicates passed (estimate = " ++ show (round est) ++ ")"

-- Test 4: large cardinality (near 2^20) with acceptable error
testLarge :: IO ()
testLarge = do
  let p = 12                 -- 4096 registers
      n = 2000000
      h0 = newHLL p
      elems = map splitmix64 [1..fromIntegral n] :: [Word64]
      h = foldr hllAdd h0 elems
      est = hllCount h
      err = abs (est - fromIntegral n) / fromIntegral n
  assert (err < 0.07) $ "Large cardinality relative error too high: " ++ printf "%.2f%%" (err*100)
  putStrLn $ "testLarge passed (estimate = " ++ show (round est) ++ ", error = " ++ printf "%.2f%%" (err*100) ++ ")"

main :: IO ()
main = do
  putStrLn "Running HyperLogLog tests..."
  testSmall
  testMedium
  testDuplicates
  testLarge
  putStrLn "All tests passed."
