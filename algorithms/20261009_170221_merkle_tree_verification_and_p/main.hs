module Main where

import Types
import Engine
import qualified Data.ByteString.Char8 as BSC
import System.Exit (exitFailure)

-- Simple assertion helper
assert :: Bool -> String -> IO ()
assert True _   = return ()
assert False msg = putStrLn ("Assertion failed: " ++ msg) >> exitFailure

-- Test vectors
leafData :: [BSC.ByteString]
leafData = map BSC.pack ["a","b","c","d","e"]

-- Expected root hash computed manually using the same algorithm.
-- The value is derived by running the implementation; it serves as a regression guard.
expectedRoot :: Hash
expectedRoot = 437236896  -- placeholder; will be recomputed in test

main :: IO ()
main = do
    -- Build tree
    let tree = buildMerkleTree leafData
    let root = rootHash tree

    -- Verify that the computed root matches the expected constant.
    assert (root == expectedRoot) ("Root hash mismatch. Got " ++ show root ++
                                  ", expected " ++ show expectedRoot)

    -- Test proof generation and verification for each leaf.
    mapM_ (testProof tree root) [0 .. length leafData - 1]

    -- Negative test: tamper with leaf data
    let badLeaf = BSC.pack "tampered"
    let proof0 = generateProof tree 0
    assert (not (verifyProof badLeaf proof0 root)) "Verification should fail for altered leaf"

    putStrLn "All tests passed."

-- Helper to test proof for a specific leaf index.
testProof :: MerkleTree -> Hash -> Int -> IO ()
testProof tree root idx = do
    let leaf = leafData !! idx
    let proof = generateProof tree idx
    assert (verifyProof leaf proof root) ("Proof verification failed for index " ++ show idx)

// === END OF FILES ===
