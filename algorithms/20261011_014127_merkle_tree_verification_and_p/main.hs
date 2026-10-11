module Main where

import Types
import Engine

import System.Exit (exitFailure)

-- | Simple hash function for Int values (identity).
intHash :: Int -> Hash
intHash = id

-- | Simple hash function for String values.
stringHash :: String -> Hash
stringHash = foldl (\h c -> hashCombine h (fromEnum c)) 0

-- | Helper to assert a condition; aborts on failure.
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = putStrLn ("Assertion failed: " ++ msg) >> exitFailure

-- | Run a suite of unit tests covering construction, proof generation and verification.
runTests :: IO ()
runTests = do
    -- Test 1: Even number of leaves
    let vals1 = [1,2,3,4] :: [Int]
        tree1 = buildMerkleTree intHash vals1
        root1 = merkleRoot tree1

    -- Manually compute expected root:
    -- level 1: h1=1, h2=2, h3=3, h4=4
    -- level 2: n12 = combine 1 2, n34 = combine 3 4
    -- level 3: root = combine n12 n34
    let n12 = hashCombine 1 2
        n34 = hashCombine 3 4
        expectedRoot1 = hashCombine n12 n34
    assert (root1 == expectedRoot1) "Root hash mismatch for even leaf count"

    -- Proof for leaf 3
    let Just proof3 = generateProof intHash tree1 3
    assert (verifyProof intHash 3 proof3 root1) "Valid proof for leaf 3 failed"

    -- Tampered leaf verification should fail
    assert (not (verifyProof intHash 99 proof3 root1)) "Tampered leaf incorrectly verified"

    -- Test 2: Odd number of leaves (duplicate last)
    let vals2 = [10,20,30] :: [Int]
        tree2 = buildMerkleTree intHash vals2
        root2 = merkleRoot tree2

    -- Manual calculation:
    -- level1: 10,20,30,30 (duplicate)
    -- level2: n1 = combine 10 20, n2 = combine 30 30
    -- root = combine n1 n2
    let n1 = hashCombine 10 20
        n2 = hashCombine 30 30
        expectedRoot2 = hashCombine n1 n2
    assert (root2 == expectedRoot2) "Root hash mismatch for odd leaf count"

    -- Proof for leaf 20
    let Just proof20 = generateProof intHash tree2 20
    assert (verifyProof intHash 20 proof20 root2) "Valid proof for leaf 20 failed"

    -- Proof for duplicated leaf 30 (should succeed)
    let Just proof30 = generateProof intHash tree2 30
    assert (verifyProof intHash 30 proof30 root2) "Valid proof for duplicated leaf 30 failed"

    -- Test 3: String values
    let strs = ["alice","bob","carol"] :: [String]
        treeStr = buildMerkleTree stringHash strs
        rootStr = merkleRoot treeStr

    let Just proofBob = generateProof stringHash treeStr "bob"
    assert (verifyProof stringHash "bob" proofBob rootStr) "String proof verification failed"

    putStrLn "All tests passed."

main :: IO ()
main = runTests
