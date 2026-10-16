module Main where

import Core
import System.Exit (exitFailure, exitSuccess)

type Test = ([Instr], Either String [Int])

tests :: [Test]
tests =
  [ ([Push 2, Push 3, Add], Right [5])
  , ([Push 10, Push 5, Sub], Right [5])
  , ([Push 4, Push 2, Mul], Right [8])
  , ([Push 9, Push 3, Div], Right [3])
  , ([Push 1, Pop], Right [])
  , ([Dup], Left "dup on empty stack")
  , ([Push 1, Swap], Left "swap needs two elements")
  , ([Push 0, Push 1, Div], Left "division by zero")
  ]

runTest :: Int -> Test -> IO Bool
runTest n (prog, expected) = do
    let result = runVM prog
    if result == expected
      then do
        putStrLn $ "Test " ++ show n ++ " passed."
        return True
      else do
        putStrLn $ "Test " ++ show n ++ " FAILED."
        putStrLn $ "  Program: " ++ show prog
        putStrLn $ "  Expected: " ++ show expected
        putStrLn $ "  Got: " ++ show result
        return False

main :: IO ()
main = do
    results <- mapM (uncurry runTest) (zip [1..] tests)
    if and results
      then do
        putStrLn "All tests passed."
        exitSuccess
      else do
        putStrLn "Some tests failed."
        exitFailure
