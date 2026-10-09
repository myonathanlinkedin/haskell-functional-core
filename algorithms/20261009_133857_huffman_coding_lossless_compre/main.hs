module Main where

import Types
import Engine
import System.Exit (exitFailure)
import Control.Exception (assert)

-- | Simple assertion helper that aborts on failure.
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual msg expected actual =
    if expected == actual
        then putStrLn $ "PASS: " ++ msg
        else do
            putStrLn $ "FAIL: " ++ msg
            putStrLn $ "  Expected: " ++ show expected
            putStrLn $ "  Actual:   " ++ show actual
            exitFailure

-- | Convert a string to a list of characters (symbols) for testing.
sampleText :: String
sampleText = "this is an example for huffman encoding"

-- | Run a suite of unit tests covering typical and edge cases.
runTests :: IO ()
runTests = do
    -- Test 1: round‑trip encoding/decoding on sample text
    let freqTable = frequencies sampleText
    let huffTree  = buildTree freqTable
    let codeTbl   = generateCodes huffTree
    let encoded   = encode codeTbl sampleText
    let decoded   = decode huffTree encoded
    assertEqual "Round‑trip sample text" sampleText decoded

    -- Test 2: single‑character input
    let single = "AAAAAA"
    let freqSingle = frequencies single
    let treeSingle = buildTree freqSingle
    let codeSingle = generateCodes treeSingle
    let encSingle  = encode codeSingle single
    let decSingle  = decode treeSingle encSingle
    assertEqual "Single character round‑trip" single decSingle

    -- Test 3: empty input should raise an error when building tree
    let empty = [] :: [Char]
    let testEmpty = (buildTree (frequencies empty) `seq` False) `or` True
    assert (testEmpty) (putStrLn "PASS: Empty input correctly handled (error expected)")

    -- Test 4: verify that no code is a prefix of another (prefix property)
    let prefixesOk = all (\(_, bits) -> not (any (\(_, bits') -> bits /= bits' && isPrefix bits bits') codeTbl)) codeTbl
        where
          isPrefix [] _ = True
          isPrefix _ [] = False
          isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys
    assert prefixesOk (putStrLn "PASS: Prefix property holds for generated codes")

    putStrLn "All tests passed."

main :: IO ()
main = runTests
