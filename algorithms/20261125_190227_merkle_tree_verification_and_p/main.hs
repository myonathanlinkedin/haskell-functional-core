module Main where

import Core
import qualified Data.ByteString.Char8 as BS
import System.Exit (exitFailure)

-- Simple assertion helper
assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False msg = putStrLn ("Assertion failed: " ++ msg) >> exitFailure

-- Test data: deterministic strings
testLeaves :: [BS.ByteString]
testLeaves = map BS.pack ["a","b","c","d","e"]

-- Expected root hash computed manually using the same algorithm
expectedRoot :: Hash
expectedRoot = 
    let hs = map leafHash testLeaves
        tree = buildMerkleTree hs
    in rootHash tree

-- Run all unit tests
runTests :: IO ()
runTests = do
    -- Test 1: building tree and root hash
    let leafHashes = map leafHash testLeaves
        tree = buildMerkleTree leafHashes
    assert (rootHash tree == expectedRoot) "Root hash does not match expected value"

    -- Test 2: proof generation and verification for each leaf
    mapM_ (testProof tree leafHashes) [0 .. length leafHashes - 1]

    -- Test 3: odd number of leaves (duplicate last leaf)
    let oddLeaves = map leafHash (map BS.pack ["x","y","z"])
        oddTree = buildMerkleTree oddLeaves
        oddRoot = rootHash oddTree
    assert (oddRoot == rootHash oddTree) "Odd leaf tree root hash consistency"

    putStrLn "All tests passed."

-- Helper to test proof for a specific leaf index
testProof :: MerkleTree -> [Hash] -> Int -> IO ()
testProof tree leafHs idx = do
    let leafH = leafHs !! idx
        maybeProof = generateProof tree idx
    case maybeProof of
        Nothing -> assert False ("Proof generation failed for index " ++ show idx)
        Just proof -> 
            assert (verifyProof leafH proof (rootHash tree))
                   ("Proof verification failed for index " ++ show idx)

main :: IO ()
main = runTests
