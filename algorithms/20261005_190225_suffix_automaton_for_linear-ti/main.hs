module Main where

import Core (SAM, buildSAM, contains, occurrences)
import System.Exit (exitFailure)
import Control.Exception (evaluate)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False msg = putStrLn ("Assertion failed: " ++ msg) >> exitFailure

runTests :: IO ()
runTests = do
  let text = "ababa"
      sam = buildSAM text

  -- existence tests
  assert (contains sam "aba") "contains aba"
  assert (contains sam "bab") "contains bab"
  assert (not (contains sam "abc")) "not contains abc"
  assert (contains sam "") "empty pattern"

  -- occurrence tests
  assert (occurrences sam "aba" == 2) "occurrences aba == 2"
  assert (occurrences sam "bab" == 1) "occurrences bab == 1"
  assert (occurrences sam "ab"  == 2) "occurrences ab == 2"
  assert (occurrences sam "a"   == 3) "occurrences a == 3"
  assert (occurrences sam "abc" == 0) "occurrences abc == 0"

  putStrLn "All unit tests passed."

benchmark :: IO ()
benchmark = do
  let large = concat $ replicate 10000 "abacabadabacaba"
  start <- getCurrentTime
  let sam = buildSAM large
  _ <- evaluate sam
  mid <- getCurrentTime
  let occ = occurrences sam "abacaba"
  _ <- evaluate occ
  end <- getCurrentTime
  putStrLn $ "Build time: " ++ show (diffUTCTime mid start)
  putStrLn $ "Query time: " ++ show (diffUTCTime end mid)
  putStrLn $ "Occurrences of \"abacaba\": " ++ show occ

main :: IO ()
main = do
  runTests
  benchmark
