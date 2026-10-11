module Main where

import Core
import System.Exit (exitFailure, exitSuccess)

-- Simple assertion helper
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual msg expected actual =
    if expected == actual
        then putStrLn $ "[PASS] " ++ msg
        else do
            putStrLn $ "[FAIL] " ++ msg ++ "\n  Expected: " ++ show expected ++ "\n  Actual:   " ++ show actual
            exitFailure

assertBool :: String -> Bool -> IO ()
assertBool msg cond = assertEqual msg True cond

-- Test suite
testEmpty :: IO ()
testEmpty = do
    let q = empty :: Queue Int
    assertBool "empty is empty" (isEmpty q)
    assertEqual "peek empty" Nothing (peek q)
    assertEqual "dequeue empty" Nothing (dequeue q)

testEnqueueDequeue :: IO ()
testEnqueueDequeue = do
    let q0 = empty :: Queue Int
        q1 = enqueue 1 q0
        q2 = enqueue 2 q1
        q3 = enqueue 3 q2
    assertBool "q3 not empty" (not $ isEmpty q3)
    assertEqual "peek q3" (Just 1) (peek q3)
    let Just (x1, q4) = dequeue q3
    assertEqual "dequeue first element" 1 x1
    assertEqual "peek after one dequeue" (Just 2) (peek q4)
    let Just (x2, q5) = dequeue q4
    assertEqual "dequeue second element" 2 x2
    let Just (x3, q6) = dequeue q5
    assertEqual "dequeue third element" 3 x3
    assertBool "queue empty after all dequeues" (isEmpty q6)

testFromToList :: IO ()
testFromToList = do
    let lst = [10,20,30,40,50]
        q = fromList lst :: Queue Int
    assertEqual "toList after fromList" lst (toList q)
    let Just (_, q') = dequeue q
    assertEqual "toList after one dequeue" (tail lst) (toList q')
    let q'' = enqueue 60 q'
    assertEqual "toList after enqueue 60" (init (tail lst) ++ [40,50,60]) (toList q'')

testBalanceInvariant :: IO ()
testBalanceInvariant = do
    let q = foldr enqueue empty [1..1000] :: Queue Int
    assertBool "large queue not empty" (not $ isEmpty q)
    let lst = toList q
    assertEqual "toList matches original order" [1..1000] lst

runAllTests :: IO ()
runAllTests = do
    testEmpty
    testEnqueueDequeue
    testFromToList
    testBalanceInvariant
    putStrLn "All tests passed."
    exitSuccess

main :: IO ()
main = runAllTests
