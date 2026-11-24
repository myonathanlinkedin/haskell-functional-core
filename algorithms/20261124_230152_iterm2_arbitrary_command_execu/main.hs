module Main where

import Core (sanitizeData, APC(..), renderAPC, isSafeCommand)
import System.Exit (exitFailure)

-- Simple assertion helper.
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual testName expected actual =
    if expected == actual
        then putStrLn $ "[PASS] " ++ testName
        else do
            putStrLn $ "[FAIL] " ++ testName
            putStrLn $ "  Expected: " ++ show expected
            putStrLn $ "  Actual  : " ++ show actual
            exitFailure

-- Test data construction helpers.
apcSeq :: String -> String -> String
apcSeq cmd pl = renderAPC (APC cmd pl)

-- Test suite.
runTests :: IO ()
runTests = do
    -- 1. Safe APC is preserved.
    let input1 = "Hello " ++ apcSeq "set" "color=blue" ++ " World"
    let expected1 = input1
    assertEqual "Safe APC preserved" expected1 (sanitizeData input1)

    -- 2. Unsafe APC is removed.
    let input2 = "Start " ++ apcSeq "exec" "rm -rf /" ++ " End"
    let expected2 = "Start  End"
    assertEqual "Unsafe APC removed" expected2 (sanitizeData input2)

    -- 3. Mixed safe and unsafe APCs.
    let input3 = apcSeq "set" "a=1" ++ " " ++ apcSeq "exec" "bad" ++ " " ++ apcSeq "clear" ""
    let expected3 = apcSeq "set" "a=1" ++ "  " ++ apcSeq "clear" ""
    assertEqual "Mixed APCs" expected3 (sanitizeData input3)

    -- 4. Malformed APC (no terminator) is left untouched except the leading ESC.
    let malformed = "\ESC_foo bar"
    let input4 = "Prefix " ++ malformed ++ " Suffix"
    let expected4 = "Prefix " ++ malformed ++ " Suffix"
    assertEqual "Malformed APC unchanged" expected4 (sanitizeData input4)

    -- 5. APC with empty payload.
    let input5 = apcSeq "reset" ""
    let expected5 = input5
    assertEqual "Empty payload safe APC" expected5 (sanitizeData input5)

    -- 6. Multiple consecutive APCs.
    let input6 = apcSeq "set" "x=1" ++ apcSeq "exec" "evil" ++ apcSeq "clear" ""
    let expected6 = apcSeq "set" "x=1" ++ apcSeq "clear" ""
    assertEqual "Consecutive APCs" expected6 (sanitizeData input6)

    -- 7. No APC at all.
    let input7 = "Just a plain string."
    let expected7 = input7
    assertEqual "No APCs" expected7 (sanitizeData input7)

    putStrLn "All tests passed."

main :: IO ()
main = runTests
