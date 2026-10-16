module Engine (
    buildMerkleTree,
    rootHash,
    generateProof,
    verifyProof,
    leafCount
) where

import Types
import qualified Data.ByteString as BS

-- | Build a Merkle tree from a list of leaf values.
--   Empty input is illegal and will raise an error.
buildMerkleTree :: [BS.ByteString] -> MerkleTree
buildMerkleTree [] = error "Cannot build Merkle tree from empty list"
buildMerkleTree xs = buildLevels (map makeLeaf xs)
  where
    makeLeaf :: BS.ByteString -> MerkleTree
    makeLeaf bs = let h = hashLeaf bs in Leaf h bs

    -- Recursively combine adjacent nodes until a single root remains.
    buildLevels :: [MerkleTree] -> MerkleTree
    buildLevels [t] = t
    buildLevels ts  = buildLevels (pairUp ts)

    -- Pair up nodes; if odd count, duplicate the last node.
    pairUp :: [MerkleTree] -> [MerkleTree]
    pairUp [] = []
    pairUp [t] = [t]  -- will be duplicated in the next recursion level
    pairUp (l:r:rest) = combine l r : pairUp rest

    combine :: MerkleTree -> MerkleTree -> MerkleTree
    combine left right =
        let h = hashCombine (nodeHash left) (nodeHash right)
        in Node h left right

    nodeHash :: MerkleTree -> Hash
    nodeHash (Leaf h _)   = h
    nodeHash (Node h _ _) = h

-- | Extract the root hash of a Merkle tree.
rootHash :: MerkleTree -> Hash
rootHash (Leaf h _)   = h
rootHash (Node h _ _) = h

-- | Count the number of leaf nodes in a tree.
leafCount :: MerkleTree -> Int
leafCount (Leaf _ _)   = 1
leafCount (Node _ l r) = leafCount l + leafCount r

-- | Generate a Merkle proof for the leaf at the given zero‑based index.
generateProof :: MerkleTree -> Int -> MerkleProof
generateProof tree idx
    | idx < 0 || idx >= leafCount tree = error "Index out of bounds"
    | otherwise = go tree idx 0
  where
    go :: MerkleTree -> Int -> Int -> MerkleProof
    go (Leaf _ _) _ _ = []  -- reached the target leaf
    go (Node _ left right) target startIdx =
        let leftSize = leafCount left
        in if target < startIdx + leftSize
           then -- target is in left subtree
                (R, nodeHash right) : go left target startIdx
           else -- target is in right subtree
                (L, nodeHash left) : go right target (startIdx + leftSize)

    nodeHash :: MerkleTree -> Hash
    nodeHash (Leaf h _)   = h
    nodeHash (Node h _ _) = h

-- | Verify a Merkle proof against a claimed root hash.
verifyProof :: BS.ByteString -> MerkleProof -> Hash -> Bool
verifyProof leafBytes proof claimedRoot =
    let startHash = hashLeaf leafBytes
        computedRoot = foldl apply startHash proof
    in computedRoot == claimedRoot
  where
    apply :: Hash -> (Direction, Hash) -> Hash
    apply curHash (L, sibling) = hashCombine sibling curHash
    apply curHash (R, sibling) = hashCombine curHash sibling
