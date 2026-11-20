module Main (main) where

import Core
import System.Exit (exitFailure)
import Control.Monad (unless)

-- Simple assertion helper
assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False msg = putStrLn ("Assertion failed: " ++ msg) >> exitFailure

-- Test vectors
testLeaves :: [String]
testLeaves = ["a","b","c","d","e"]

-- Expected root computed with the same hash function
expectedRoot :: Hash
expectedRoot = merkleRoot (buildMerkleTree testLeaves)

-- Test building the tree and root extraction
testBuildRoot :: IO ()
testBuildRoot = do
    let tree = buildMerkleTree testLeaves
    assert (merkleRoot tree == expectedRoot) "Root hash mismatch"

-- Test proof generation and verification for each leaf
testProofs :: IO ()
testProofs = mapM_ testIdx [0 .. length testLeaves - 1]
  where
    tree = buildMerkleTree testLeaves
    root = merkleRoot tree
    testIdx i = case generateProof tree i of
        Nothing -> assert False ("No proof for index " ++ show i)
        Just proof -> do
            let leafVal = testLeaves !! i
                ok = verifyProof root leafVal proof
            assert ok ("Proof verification failed for index " ++ show i)

-- Edge case: single leaf tree
testSingleLeaf :: IO ()
testSingleLeaf = do
    let leaf = ["only"]
        tree = buildMerkleTree leaf
        root = merkleRoot tree
    case generateProof tree 0 of
        Nothing -> assert False "Proof missing for single leaf"
        Just proof -> assert (verifyProof root (head leaf) proof) "Single leaf proof invalid"

-- Edge case: duplicate handling for odd number of leaves
testOddDuplication :: IO ()
testOddDuplication = do
    let oddLeaves = ["x","y","z"]  -- three leaves, last duplicated internally
        tree = buildMerkleTree oddLeaves
        root = merkleRoot tree
    -- Verify proof for each original leaf
    mapM_ (\i -> case generateProof tree i of
            Nothing -> assert False ("Missing proof for odd leaf " ++ show i)
            Just p  -> assert (verifyProof root (oddLeaves !! i) p) ("Odd leaf proof failed " ++ show i)
          ) [0..2]

-- Run all tests
runTests :: IO ()
runTests = do
    testBuildRoot
    testProofs
    testSingleLeaf
    testOddDuplication
    putStrLn "All tests passed."

main :: IO ()
main = runTests
