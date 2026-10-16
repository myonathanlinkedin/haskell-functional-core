module Types (
    Hash,
    MerkleTree(..),
    Direction(..),
    MerkleProof,
    hashLeaf,
    hashCombine
) where

import qualified Data.ByteString as BS
import Data.Word (Word8)

-- | Simple deterministic integer hash (modular arithmetic)
type Hash = Integer

prime :: Integer
prime = 1000000007  -- a large prime for modulo reduction

-- | Hash a leaf value (ByteString) into a Hash.
hashLeaf :: BS.ByteString -> Hash
hashLeaf bs = BS.foldl' step 0 bs
  where
    step :: Hash -> Word8 -> Hash
    step h w = (h * 256 + fromIntegral w) `mod` prime

-- | Combine two child hashes into a parent hash.
hashCombine :: Hash -> Hash -> Hash
hashCombine h1 h2 = (h1 * 31 + h2) `mod` prime

-- | Binary Merkle tree. Each node stores its hash.
data MerkleTree
    = Leaf Hash BS.ByteString
    | Node Hash MerkleTree MerkleTree
    deriving (Show, Eq)

-- | Direction of a sibling hash in a proof.
data Direction = L | R deriving (Show, Eq)

-- | A Merkle proof is a list of (Direction, siblingHash) from leaf up to root.
type MerkleProof = [(Direction, Hash)]
