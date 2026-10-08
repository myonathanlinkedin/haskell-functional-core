module Main where

import Core
import System.Exit (exitFailure, exitSuccess)

assert :: Bool -> String -> IO ()
assert True _   = return ()
assert False m = putStrLn ("FAIL: " ++ m) >> exitFailure

testBuild :: IO ()
testBuild = do
    let leaves = ["a", "b", "c", "d"]
        tree   = buildMerkleTree leaves
        rh1    = rootHash tree
        rh2    = rootHash (buildMerkleTree leaves)
    assert (rh1 == rh2) "Root hash should be deterministic and consistent"

testProof :: IO ()
testProof = do
    let leaves = ["a", "b", "c", "d"]
        tree   = buildMerkleTree leaves
        rh     = rootHash tree
    case merkleProof tree 2 of
        Nothing    -> assert False "Proof generation failed for index 2"
        Just proof -> do
            let ok = verifyProof "c" proof rh
            assert ok "Verification of proof for leaf \"c\" failed"

testOdd :: IO ()
testOdd = do
    let leaves = ["a", "b", "c"]
        tree   = buildMerkleTree leaves
        rh     = rootHash tree
    case merkleProof tree 2 of
        Nothing    -> assert False "Proof generation failed for odd number of leaves"
        Just proof -> do
            let ok = verifyProof "c" proof rh
            assert ok "Verification of proof for leaf \"c\" in odd tree failed"

main :: IO ()
main = do
    testBuild
    testProof
    testOdd
    putStrLn "All tests passed."
    exitSuccess
