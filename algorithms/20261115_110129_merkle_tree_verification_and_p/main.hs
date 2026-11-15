module Main where

import Core
import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

-- Sample data for testing
sampleLeaves :: [String]
sampleLeaves = ["a", "b", "c", "d", "e"]

-- Expected root hash computed manually using the same hash functions.
expectedRoot :: Int
expectedRoot = rootHash $ buildMerkleTree sampleLeaves

-- Test building a Merkle tree and checking its root hash.
testBuildRoot :: Test
testBuildRoot = TestCase $ do
    let tree = buildMerkleTree sampleLeaves
    assertEqual "Root hash should match expected value" expectedRoot (rootHash tree)

-- Test proof generation and verification for an existing leaf.
testProofValid :: Test
testProofValid = TestCase $ do
    let tree = buildMerkleTree sampleLeaves
    let leaf = "c"
    let mProof = generateProof tree leaf
    case mProof of
        Nothing -> assertFailure "Proof should be generated for existing leaf."
        Just proof -> assertBool "Valid proof should verify" (verifyProof (rootHash tree) leaf proof)

-- Test that verification fails for an incorrect leaf value.
testProofInvalidLeaf :: Test
testProofInvalidLeaf = TestCase $ do
    let tree = buildMerkleTree sampleLeaves
    let leaf = "c"
    let wrongLeaf = "z"
    let mProof = generateProof tree leaf
    case mProof of
        Nothing -> assertFailure "Proof should be generated for existing leaf."
        Just proof -> assertBool "Proof should not verify for wrong leaf" (not $ verifyProof (rootHash tree) wrongLeaf proof)

-- Test that verification fails for a tampered proof.
testProofTampered :: Test
testProofTampered = TestCase $ do
    let tree = buildMerkleTree sampleLeaves
    let leaf = "b"
    let mProof = generateProof tree leaf
    case mProof of
        Nothing -> assertFailure "Proof should be generated for existing leaf."
        Just proof -> do
            let tampered = case proof of
                    [] -> []  -- cannot tamper empty proof
                    ((h,dir):rest) -> (h+1, dir) : rest
            assertBool "Tampered proof should not verify" (not $ verifyProof (rootHash tree) leaf tampered)

-- Test that requesting a proof for a non‑existent leaf returns Nothing.
testProofMissing :: Test
testProofMissing = TestCase $ do
    let tree = buildMerkleTree sampleLeaves
    let mProof = generateProof tree "nonexistent"
    assertEqual "Proof for missing leaf should be Nothing" Nothing mProof

tests :: Test
tests = TestList
    [ TestLabel "BuildRoot" testBuildRoot
    , TestLabel "ProofValid" testProofValid
    , TestLabel "ProofInvalidLeaf" testProofInvalidLeaf
    , TestLabel "ProofTampered" testProofTampered
    , TestLabel "ProofMissing" testProofMissing
    ]

main :: IO ()
main = do
    counts <- runTestTT tests
    if failures counts > 0 || errors counts > 0
        then exitFailure
        else exitSuccess
