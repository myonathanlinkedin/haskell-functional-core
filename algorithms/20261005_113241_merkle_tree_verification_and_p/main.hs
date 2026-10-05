module Main where

import Core
import qualified Data.ByteString.Char8 as BSC

-- Build a sample Merkle tree from integers 1..7
testTree :: MerkleTree Int
testTree = buildMerkleTree [1..7] (BSC.pack . show)

runTest :: IO ()
runTest = case rootHash testTree of
    Nothing -> putStrLn "FAIL: empty tree"
    Just root -> case generateProof testTree 4 of
        Nothing   -> putStrLn "FAIL: proof not found"
        Just proof ->
            if verifyProof root 4 (BSC.pack . show) proof
                then putStrLn "PASS: proof verified"
                else putStrLn "FAIL: proof verification failed"

main :: IO ()
main = do
    putStrLn "Merkle Tree Verification Tests"
    runTest
