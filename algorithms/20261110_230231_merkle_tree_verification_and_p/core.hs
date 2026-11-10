module Core (
    Hash,
    MerkleTree(..),
    Direction(..),
    ProofStep(..),
    buildMerkleTree,
    rootHash,
    mkProof,
    verifyProof,
    hashLeaf,
    hashNode
) where

import qualified Data.ByteString.Char8 as BS
import Data.Hashable (hash)
import Data.List (genericLength)

-- | Simple deterministic hash type using Int.
type Hash = Int

-- | Binary Merkle tree where each node stores its hash.
data MerkleTree
    = Leaf Hash
    | Node Hash MerkleTree MerkleTree
    deriving (Show, Eq)

-- | Direction of a sibling in a proof.
data Direction = L | R deriving (Show, Eq)

-- | One step of a Merkle proof: sibling hash and its direction.
data ProofStep = ProofStep
    { siblingHash :: Hash
    , direction   :: Direction
    } deriving (Show, Eq)

-- | Hash a leaf value (ByteString) to a Hash.
hashLeaf :: BS.ByteString -> Hash
hashLeaf = hash

-- | Combine two child hashes into a parent hash.
hashNode :: Hash -> Hash -> Hash
hashNode l r = hash (show l ++ show r)

-- | Extract the stored hash from a tree node.
nodeHash :: MerkleTree -> Hash
nodeHash (Leaf h)       = h
nodeHash (Node h _ _)   = h

-- | Build a Merkle tree from a list of leaf values.
--   The list is padded by repeating the last element until its length
--   is a power of two. Empty input is an error.
buildMerkleTree :: [BS.ByteString] -> MerkleTree
buildMerkleTree [] = error "Cannot build Merkle tree from empty list"
buildMerkleTree xs = buildTree paddedLeaves
  where
    paddedLeaves = map Leaf $ map hashLeaf $ padToPowerOfTwo xs

-- | Compute the root hash of a Merkle tree.
rootHash :: MerkleTree -> Hash
rootHash = nodeHash

-- | Generate a Merkle proof for the leaf at the given zero‑based index.
mkProof :: MerkleTree -> Int -> [ProofStep]
mkProof tree idx = go tree idx (treeSize tree)
  where
    go :: MerkleTree -> Int -> Int -> [ProofStep]
    go (Leaf _) 0 _ = []
    go (Node _ l r) i size
        | i < half = ProofStep (nodeHash r) R : go l i half
        | otherwise = ProofStep (nodeHash l) L : go r (i - half) half
      where
        half = size `div` 2
    go _ _ _ = error "Invalid index for proof generation"

-- | Verify a Merkle proof.
--   Takes the leaf hash, the proof steps, and the expected root hash.
verifyProof :: Hash -> [ProofStep] -> Hash -> Bool
verifyProof leafHash steps expectedRoot = computedRoot == expectedRoot
  where
    computedRoot = foldl combine leafHash steps
    combine :: Hash -> ProofStep -> Hash
    combine cur (ProofStep sib L) = hashNode sib cur
    combine cur (ProofStep sib R) = hashNode cur sib

-- ----------------------------------------------------------------------
-- Helper functions (pure)

-- | Pad a list by repeating its last element until its length is a power of two.
padToPowerOfTwo :: [a] -> [a]
padToPowerOfTwo xs = xs ++ replicate (nextPow2 n - n) (last xs)
  where
    n = length xs
    nextPow2 k
        | k <= 1    = 1
        | otherwise = let p = ceiling (logBase 2 (fromIntegral k :: Double))
                       in 2 ^ p

-- | Number of leaves represented by a (possibly padded) tree.
treeSize :: MerkleTree -> Int
treeSize (Leaf _)       = 1
treeSize (Node _ l _) = 2 * treeSize l  -- left and right subtrees are equal size

-- | Build the tree from a list of leaf nodes (already hashed).
buildTree :: [MerkleTree] -> MerkleTree
buildTree [t] = t
buildTree ts  = buildTree (pairUp ts)
  where
    pairUp :: [MerkleTree] -> [MerkleTree]
    pairUp [] = []
    pairUp (a:b:rest) = let h = hashNode (nodeHash a) (nodeHash b)
                         in Node h a b : pairUp rest
    pairUp [_] = error "pairUp received odd number of nodes; should be impossible after padding"
