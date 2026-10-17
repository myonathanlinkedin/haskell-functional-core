module Main where

import MerkleTree
import Data.List (intercalate)

-- Simple assertion helper
assert :: Bool -> String -> IO ()
assert True _  = return ()
assert False msg = error ("Assertion failed: " ++ msg)

-- Test building a tree
testBuild :: IO ()
testBuild = do
  let leaves = ["a","b","c","d"]
  let tree = buildMerkleTree leaves
  let root = rootHash tree
  assert (root /= 0) "Root hash should not be zero"

-- Test generating and verifying proofs
testProof :: IO ()
testProof = do
  let leaves = ["a","b","c","d"]
  let tree = buildMerkleTree leaves
  let root = rootHash tree
  mapM_ (\i -> do
    let p = proof tree i
    let leaf = leaves !! i
    let ok = verifyProof p i leaf root
    assert ok ("Proof failed for index " ++ show i)
    ) [0..3]

-- Test that an invalid leaf fails verification
testInvalid :: IO ()
testInvalid = do
  let leaves = ["a","b","c","d"]
  let tree = buildMerkleTree leaves
  let root = rootHash tree
  let p = proof tree 0
  let ok = verifyProof p 0 "wrong" root
  assert (not ok) "Invalid proof should fail"

-- Test empty list
testEmpty :: IO ()
testEmpty = do
  let tree = buildMerkleTree []
  let root = rootHash tree
  assert (root == 0) "Root of empty tree should be 0"

main :: IO ()
main = do
  putStrLn "Running Merkle Tree tests..."
  testBuild
  testProof
  testInvalid
  testEmpty
  putStrLn "All tests passed."
