module Main where

import Core
import Data.List (intercalate)

-- Simple assertion framework
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual testName expected actual =
  if expected == actual
     then putStrLn $ "[PASS] " ++ testName
     else putStrLn $ "[FAIL] " ++ testName ++
                     "\n  Expected: " ++ show expected ++
                     "\n  Actual:   " ++ show actual

-- Test cases
runTests :: IO ()
runTests = do
  let s = "ababa"
  let sam = buildSAM s

  -- Substring existence tests
  assertEqual "isSubstring 'a'" True (isSubstring sam "a")
  assertEqual "isSubstring 'b'" True (isSubstring sam "b")
  assertEqual "isSubstring 'ab'" True (isSubstring sam "ab")
  assertEqual "isSubstring 'ba'" True (isSubstring sam "ba")
  assertEqual "isSubstring 'aba'" True (isSubstring sam "aba")
  assertEqual "isSubstring 'bab'" True (isSubstring sam "bab")
  assertEqual "isSubstring 'abc'" False (isSubstring sam "abc")
  assertEqual "isSubstring ''" True (isSubstring sam "")

  -- Occurrence count tests
  assertEqual "occ 'a'" 3 (substringOccurrences sam "a")
  assertEqual "occ 'b'" 2 (substringOccurrences sam "b")
  assertEqual "occ 'ab'" 2 (substringOccurrences sam "ab")
  assertEqual "occ 'ba'" 2 (substringOccurrences sam "ba")
  assertEqual "occ 'aba'" 2 (substringOccurrences sam "aba")
  assertEqual "occ 'bab'" 1 (substringOccurrences sam "bab")
  assertEqual "occ 'abab'" 1 (substringOccurrences sam "abab")
  assertEqual "occ 'baba'" 1 (substringOccurrences sam "baba")
  assertEqual "occ 'ababa'" 1 (substringOccurrences sam "ababa")
  assertEqual "occ 'abc'" 0 (substringOccurrences sam "abc")

  -- Edge cases
  let samEmpty = buildSAM ""
  assertEqual "isSubstring '' on empty" True (isSubstring samEmpty "")
  assertEqual "isSubstring 'a' on empty" False (isSubstring samEmpty "a")
  assertEqual "occ 'a' on empty" 0 (substringOccurrences samEmpty "a")

main :: IO ()
main = do
  putStrLn "Running Suffix Automaton tests..."
  runTests
  putStrLn "All tests completed."
