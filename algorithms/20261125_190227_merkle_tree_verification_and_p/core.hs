module Core (
    Hash,
    MerkleTree(..),
    Direction(..),
    MerkleProof,
    leafHash,
    hashCombine,
    buildMerkleTree,
    rootHash,
    generateProof,
    verifyProof
) where

import qualified Data.ByteString.Char8 as BS
import Data.Bits (shiftL, xor)

-- | Simple deterministic hash type (32‑bit unsigned integer)
type Hash = Int

-- | Direction of sibling in a proof: the sibling is on the Left (L) or Right (R) of the node.
data Direction = L | R deriving (Show, Eq)

-- | Merkle tree definition. Each node stores its hash for O(1) root retrieval.
data MerkleTree
    = Leaf Hash
    | Node Hash MerkleTree MerkleTree
    deriving (Show, Eq)

-- | A Merkle proof is a list of sibling hashes together with the direction
--   needed to reconstruct the path to the root.
type MerkleProof = [(Hash, Direction)]

-- | Very simple rolling hash over a ByteString.
leafHash :: BS.ByteString -> Hash
leafHash = BS.foldl' (\h b -> ((h `shiftL` 5) `xor` fromEnum b) .&. 0xFFFFFFFF) 0

-- | Combine two child hashes into a parent hash.
hashCombine :: Hash -> Hash -> Hash
hashCombine a b = ((a * 31) `xor` b) .&. 0xFFFFFFFF

-- | Retrieve the stored hash of a tree node.
nodeHash :: MerkleTree -> Hash
nodeHash (Leaf h)   = h
nodeHash (Node h _ _) = h

-- | Build a balanced Merkle tree from a list of leaf hashes.
--   If the list is empty, error. If the number of leaves is odd,
--   the last leaf is duplicated to keep the tree full.
buildMerkleTree :: [Hash] -> MerkleTree
buildMerkleTree [] = error "Cannot build Merkle tree from empty list"
buildMerkleTree hs = build (map Leaf hs')
  where
    hs' = if odd (length hs) then hs ++ [last hs] else hs
    build [t] = t
    build ts  = build (pairUp ts)

    pairUp :: [MerkleTree] -> [MerkleTree]
    pairUp [] = []
    pairUp [t] = [t]  -- should not happen due to duplication rule
    pairUp (a:b:rest) =
        let h = hashCombine (nodeHash a) (nodeHash b)
        in Node h a b : pairUp rest

-- | Extract the root hash of a Merkle tree.
rootHash :: MerkleTree -> Hash
rootHash = nodeHash

-- | Generate a Merkle proof for the leaf at the given zero‑based index.
--   Returns Nothing if the index is out of bounds.
generateProof :: MerkleTree -> Int -> Maybe MerkleProof
generateProof tree idx = go tree idx 0 (leafCount tree)
  where
    leafCount :: MerkleTree -> Int
    leafCount (Leaf _) = 1
    leafCount (Node _ l r) = leafCount l + leafCount r

    go :: MerkleTree -> Int -> Int -> Int -> Maybe MerkleProof
    go (Leaf _) i cur total
        | i == cur && total == 1 = Just []
        | otherwise = Nothing
    go (Node _ l r) i cur total = 
        let leftSize = leafCount l
        in if i < cur + leftSize
           then -- go left, sibling is right child
                fmap ((nodeHash r, R) :) (go l i cur leftSize)
           else -- go right, sibling is left child
                fmap ((nodeHash l, L) :) (go r i (cur + leftSize) (total - leftSize))

-- | Verify a Merkle proof against an expected root hash.
verifyProof :: Hash -> MerkleProof -> Hash -> Bool
verifyProof leafHash0 proof expectedRoot = 
    let recomputed = foldl step leafHash0 proof
    in recomputed == expectedRoot
  where
    step :: Hash -> (Hash, Direction) -> Hash
    step acc (sibling, dir) = case dir of
        L -> hashCombine sibling acc
        R -> hashCombine acc sibling
