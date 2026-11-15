module Main where

import Core
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Simple assertion helper
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual msg expected actual =
  if expected == actual
    then return ()
    else error $ msg ++ "\nExpected: " ++ show expected ++ "\nActual:   " ++ show actual

-- Test 1: Simple graph with a loop
testLoop :: IO ()
testLoop = do
  let graph = Map.fromList
        [ (1, [2])
        , (2, [3])
        , (3, [2])
        ]
      transfer :: TransferFunction
      transfer s = Set.insert 1 (Set.insert 2 (Set.insert 3 s))
      meet :: Meet
      meet = Set.intersection
      initVal = Set.empty
      result = kildall graph transfer meet initVal
      expected = Map.fromList
        [ (1, Set.fromList [1])
        , (2, Set.fromList [1,2,3])
        , (3, Set.fromList [1,2,3])
        ]
  assertEqual "Test Loop" expected result

-- Test 2: Disconnected node
testDisconnected :: IO ()
testDisconnected = do
  let graph = Map.fromList
        [ (1, [])
        , (2, [])
        ]
      transfer :: TransferFunction
      transfer s = Set.insert 1 s
      meet :: Meet
      meet = Set.union
      initVal = Set.empty
      result = kildall graph transfer meet initVal
      expected = Map.fromList
        [ (1, Set.fromList [1])
        , (2, Set.empty)
        ]
  assertEqual "Test Disconnected" expected result

-- Test 3: Identity transfer, union meet
testIdentity :: IO ()
testIdentity = do
  let graph = Map.fromList
        [ (1, [2])
        , (2, [3])
        , (3, [])
        ]
      transfer :: TransferFunction
      transfer = id
      meet :: Meet
      meet = Set.union
      initVal = Set.empty
      result = kildall graph transfer meet initVal
      expected = Map.fromList
        [ (1, Set.empty)
        , (2, Set.empty)
        , (3, Set.empty)
        ]
  assertEqual "Test Identity" expected result

runTests :: IO ()
runTests = do
  testLoop
  testDisconnected
  testIdentity
  putStrLn "All tests passed."

main :: IO ()
main = runTests
