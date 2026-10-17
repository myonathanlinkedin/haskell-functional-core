module Main where

import Core
import Data.List (nub)
import System.Exit (exitFailure)

-- Simple test harness
assert :: Bool -> String -> IO ()
assert True _   = return ()
assert False msg = putStrLn ("FAIL: " ++ msg) >> exitFailure

-- Test that encoding then decoding yields the original data.
prop_roundTrip :: (Ord a, Show a) => [a] -> IO ()
prop_roundTrip xs = do
    let encoded = encode xs
        tree = buildTree (frequencies xs)
        decoded = decode tree encoded
    assert (decoded == xs) ("Round‑trip failed on " ++ show xs)

-- Test that generated codes are prefix‑free.
prop_prefixFree :: (Ord a, Show a) => [a] -> IO ()
prop_prefixFree xs = do
    let tree = buildTree (frequencies xs)
        codes = generateCodes tree
        codeList = map snd codes
        prefixes = [ (c1,c2) | c1 <- codeList, c2 <- codeList, c1 /= c2, c1 `isPrefixOf` c2 ]
    assert (null prefixes) ("Prefix violation on " ++ show xs)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- Edge case tests
testEmpty :: IO ()
testEmpty = do
    let encoded = encode ([] :: [Char])
        tree = buildTree (frequencies ([] :: [Char]))
        decoded = decode tree encoded
    assert (null encoded) "Empty input should encode to empty bit list"
    assert (null decoded) "Empty input should decode to empty list"

testSingleSymbol :: IO ()
testSingleSymbol = do
    let xs = replicate 10 'a'
        encoded = encode xs
        tree = buildTree (frequencies xs)
        decoded = decode tree encoded
    assert (decoded == xs) "Single‑symbol round‑trip failed"
    -- In a single‑symbol tree, all bits should be the same (e.g., all False)
    assert (all (== False) encoded || all (== True) encoded) "Single‑symbol encoding should use a uniform bit"

-- Run a suite of tests
main :: IO ()
main = do
    putStrLn "Running Huffman coding tests..."
    testEmpty
    testSingleSymbol
    prop_roundTrip "hello world"
    prop_roundTrip [1,2,3,2,1,1,4,5,5,5,5]
    prop_prefixFree "the quick brown fox jumps over the lazy dog"
    prop_prefixFree [True, False, True, True, False, False, True]
    putStrLn "All tests passed."
