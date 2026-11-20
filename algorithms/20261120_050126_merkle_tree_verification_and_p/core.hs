module Core (
    Hash,
    Direction(..),
    MerkleTree(..),
    buildMerkleTree,
    merkleRoot,
    generateProof,
    verifyProof
) where

import Data.List (foldl')
import Data.Maybe (fromMaybe)

-- | Simple deterministic hash function.
--   Not cryptographically secure; sufficient for algorithmic demonstration.
type Hash = String

hash :: String -> Hash
hash = show . foldl' step 0
  where
    prime = 1000000007 :: Int
    step h c = (h * 31 + fromEnum c) `mod` prime

-- | Direction of sibling hash in a Merkle proof.
data Direction = L | R deriving (Show, Eq)

-- | Merkle tree data structure.
data MerkleTree
    = Leaf { leafHash :: Hash }
    | Node { nodeHash :: Hash, left :: MerkleTree, right :: MerkleTree }
    deriving (Show, Eq)

-- | Retrieve the stored hash of a tree node.
treeHash :: MerkleTree -> Hash
treeHash (Leaf h)   = h
treeHash (Node h _ _) = h

-- | Build a Merkle tree from a list of leaf values.
--   If the list is empty, error.
--   If the number of leaves is odd, the last leaf is duplicated.
buildMerkleTree :: [String] -> MerkleTree
buildMerkleTree [] = error "Cannot build Merkle tree from empty list"
buildMerkleTree xs = build levels
  where
    -- initial leaf nodes
    leaves = map (Leaf . hash) xs

    -- ensure even number of nodes at each level by duplicating last if needed
    makeEven :: [MerkleTree] -> [MerkleTree]
    makeEven ys = if odd (length ys) then ys ++ [last ys] else ys

    -- build parent level from child level
    parentLevel :: [MerkleTree] -> [MerkleTree]
    parentLevel [] = []
    parentLevel (a:b:rest) =
        let h = hash (treeHash a ++ treeHash b)
        in Node h a b : parentLevel rest
    parentLevel [_] = error "Unexpected odd number of nodes after makeEven"

    -- repeatedly build levels until a single root remains
    levels :: [MerkleTree]
    levels = iterate (parentLevel . makeEven) leaves !! depth

    depth :: Int
    depth = ceiling (logBase 2 (fromIntegral (length (makeEven leaves)))) :: Int

    build :: [MerkleTree] -> MerkleTree
    build [t] = t
    build ts  = build (parentLevel (makeEven ts))

-- | Extract the root hash of a Merkle tree.
merkleRoot :: MerkleTree -> Hash
merkleRoot = treeHash

-- | Generate a Merkle proof for the leaf at the given zero‑based index.
--   Returns Nothing if the index is out of bounds.
generateProof :: MerkleTree -> Int -> Maybe [(Hash, Direction)]
generateProof tree idx = go tree idx 0 (leafCount tree)
  where
    leafCount :: MerkleTree -> Int
    leafCount (Leaf _) = 1
    leafCount (Node _ l r) = leafCount l + leafCount r

    go :: MerkleTree -> Int -> Int -> Int -> Maybe [(Hash, Direction)]
    go (Leaf _) target cur total
        | target == cur = Just []
        | otherwise     = Nothing
    go (Node _ l r) target cur total = 
        let leftSize = leafCount l
        in if target < cur + leftSize
           then -- go left, sibling is right child
                fmap ((treeHash r, R) :) (go l target cur leftSize)
           else -- go right, sibling is left child
                fmap ((treeHash l, L) :) (go r target (cur + leftSize) (total - leftSize))

-- | Verify a Merkle proof.
--   Given the expected root hash, the original leaf value, the proof,
--   returns True iff the proof reconstructs the root.
verifyProof :: Hash -> String -> [(Hash, Direction)] -> Bool
verifyProof expectedRoot leafVal proof = computedRoot == expectedRoot
  where
    leafH = hash leafVal
    computedRoot = foldl' combine leafH proof

    combine :: Hash -> (Hash, Direction) -> Hash
    combine acc (siblingHash, dir) = case dir of
        L -> hash (siblingHash ++ acc)
        R -> hash (acc ++ siblingHash)
