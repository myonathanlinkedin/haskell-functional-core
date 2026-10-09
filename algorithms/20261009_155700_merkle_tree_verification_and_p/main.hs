module Main where

import Types
import Engine
import qualified Data.ByteString.Char8 as BSC
import Control.Exception (assert)
import System.Exit (exitFailure)

-- Helper to display a hash as hex (for debugging)
hashToHex :: Hash -> String
hashToHex = concatMap (printf "%02x") . BSC.unpack
  where
    printf = BSC.unpack . BSC.pack . show

-- Simple test runner
runTests :: IO ()
runTests = do
    -- Test 1: even number of leaves
    let leaves1 = map BSC.pack ["a","b","c","d"]
        tree1   = buildMerkleTree leaves1
        root1   = treeRoot tree1

    -- Verify that each leaf's proof succeeds
    mapM_ (\(i, leaf) -> 
        let leafH = hashLeaf leaf
            mProof = generateProof tree1 i
        in case mProof of
            Nothing -> error $ "Proof generation failed for index " ++ show i
            Just proof -> 
                assert (verifyProof leafH proof root1) 
                       (return ())) 
        (zip [0..] leaves1)

    -- Test 2: odd number of leaves (duplicate last leaf internally)
    let leaves2 = map BSC.pack ["x","y","z"]
        tree2   = buildMerkleTree leaves2
        root2   = treeRoot tree2

    mapM_ (\(i, leaf) -> 
        let leafH = hashLeaf leaf
            mProof = generateProof tree2 i
        in case mProof of
            Nothing -> error $ "Proof generation failed for index " ++ show i
            Just proof -> 
                assert (verifyProof leafH proof root2) 
                       (return ())) 
        (zip [0..] leaves2)

    -- Test 3: verification must fail with wrong leaf
    let wrongLeaf = hashLeaf (BSC.pack "wrong")
        Just proof0 = generateProof tree1 0
    assert (not (verifyProof wrongLeaf proof0 root1)) (return ())

    -- Test 4: out‑of‑bounds proof request returns Nothing
    assert (generateProof tree1 10 == Nothing) (return ())

    putStrLn "All tests passed."

main :: IO ()
main = runTests
