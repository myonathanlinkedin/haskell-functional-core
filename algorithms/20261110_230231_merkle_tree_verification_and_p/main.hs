module Main where

import Core
import qualified Data.ByteString.Char8 as BS
import System.Exit (exitFailure)

-- Simple assertion helper.
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = putStrLn ("Assertion failed: " ++ msg) >> exitFailure

-- Test 1: Four leaves, even number.
testEven :: IO ()
testEven = do
    let leaves = map BS.pack ["a","b","c","d"]
        tree   = buildMerkleTree leaves
        root   = rootHash tree
    -- Generate and verify proof for each leaf.
    mapM_ (\(i, leaf) -> do
        let leafH = hashLeaf leaf
            proof = mkProof tree i
            ok    = verifyProof leafH proof root
        assert ok ("Even test failed for leaf index " ++ show i)
        ) (zip [0..] leaves)

-- Test 2: Single leaf.
testSingle :: IO ()
testSingle = do
    let leaf = BS.pack "only"
        tree = buildMerkleTree [leaf]
        root = rootHash tree
        leafH = hashLeaf leaf
        proof = mkProof tree 0
    assert (null proof) "Proof for single leaf should be empty"
    assert (verifyProof leafH proof root) "Verification failed for single leaf"

-- Test 3: Odd number of leaves (3) -> padding.
testOdd :: IO ()
testOdd = do
    let leaves = map BS.pack ["x","y","z"]
        tree   = buildMerkleTree leaves
        root   = rootHash tree
        paddedLeaves = leaves ++ [last leaves]  -- after padding we have 4 leaves
    mapM_ (\(i, leaf) -> do
        let leafH = hashLeaf leaf
            proof = mkProof tree i
            ok    = verifyProof leafH proof root
        assert ok ("Odd test failed for leaf index " ++ show i)
        ) (zip [0..] leaves)
    -- Verify that the padded duplicate also validates.
    let dupLeaf = last leaves
        dupIdx  = length leaves  -- index of the padded duplicate
        dupH    = hashLeaf dupLeaf
        dupProof = mkProof tree dupIdx
    assert (verifyProof dupH dupProof root) "Verification failed for padded duplicate leaf"

-- Test 4: Invalid proof detection.
testInvalid :: IO ()
testInvalid = do
    let leaves = map BS.pack ["a","b","c","d"]
        tree   = buildMerkleTree leaves
        root   = rootHash tree
        leafH  = hashLeaf (head leaves)
        proof  = mkProof tree 0
        tamperedProof = case proof of
            [] -> []
            (step:rest) -> step { siblingHash = siblingHash step + 1 } : rest
    assert (not (verifyProof leafH tamperedProof root)) "Tampered proof should not verify"

main :: IO ()
main = do
    putStrLn "Running Merkle Tree tests..."
    testEven
    testSingle
    testOdd
    testInvalid
    putStrLn "All tests passed."
