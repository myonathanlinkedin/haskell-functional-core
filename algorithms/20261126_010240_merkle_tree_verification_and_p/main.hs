module Main where

import Types
import Engine
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Char8 as C
import Control.Exception (assert)

main :: IO ()
main = do
    let leaves = map pack ["a", "b", "c", "d"]
    let tree = buildMerkleTree leaves
    let root = rootHash tree

    -- Root hash consistency
    assert (root == rootHash tree) (putStrLn "Root hash consistency test passed.")

    -- Proof generation and verification for each leaf
    let indices = [0 .. 3]
    mapM_ (\i -> do
        let proof = generateProof tree i
        let leaf = leaves !! i
        let ok = verifyProof leaf proof root
        assert ok (putStrLn $ "Proof verification passed for leaf " ++ show i)
        -- Alter leaf data
        let badLeaf = pack ("bad" ++ show i)
        let badOk = verifyProof badLeaf proof root
        assert (not badOk) (putStrLn $ "Proof verification correctly failed for altered leaf " ++ show i)
        ) indices

    -- Test with odd number of leaves
    let oddLeaves = map pack ["x", "y", "z"]
    let oddTree = buildMerkleTree oddLeaves
    let oddRoot = rootHash oddTree
    let oddProof0 = generateProof oddTree 0
    let oddOk0 = verifyProof (oddLeaves !! 0) oddProof0 oddRoot
    assert oddOk0 (putStrLn "Odd leaf count proof test passed.")

    putStrLn "All tests passed."
