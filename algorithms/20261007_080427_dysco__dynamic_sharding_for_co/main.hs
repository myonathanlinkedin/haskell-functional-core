module Main where

import Core
import System.Exit (exitFailure, exitSuccess)

-- Simple assertion helper.
assert :: Bool -> String -> IO ()
assert True _    = return ()
assert False msg = putStrLn ("Assertion failed: " ++ msg) >> exitFailure

main :: IO ()
main = do
    testSplit
    testProcess
    testCombine
    putStrLn "All tests passed."
    exitSuccess

testSplit :: IO ()
testSplit = do
    let tokens = ["a","b","c","d","e"]
        plan   = splitTokens 3 tokens
        expected = [(Edge, ["a","b","c"]), (Cloud, ["d","e"])]
    assert (plan == expected) "splitTokens failed"

testProcess :: IO ()
testProcess = do
    let edgeRes = processShard Edge ["hello","world"]
        expected = ["Edge:HELLO","Edge:WORLD"]
    assert (edgeRes == expected) "processShard Edge failed"

testCombine :: IO ()
testCombine = do
    let plan = [(Edge, ["x","y"]), (Cloud, ["z"])]
        edgeOut  = processShard Edge ["x","y"]
        cloudOut = processShard Cloud ["z"]
        combined = combineResults plan [edgeOut, cloudOut]
        expected = ["Edge:X","Edge:Y","Cloud:Z"]
    assert (combined == expected) "combineResults failed"
