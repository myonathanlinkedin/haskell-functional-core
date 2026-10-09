module Types where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

-- | A hash is represented as a ByteString.
type Hash = ByteString

-- | Merkle tree data structure.
--   Each node stores its hash for O(1) root retrieval.
data MerkleTree
    = Leaf { leafHash :: Hash }
    | Node { nodeHash :: Hash, leftChild :: MerkleTree, rightChild :: MerkleTree }
    deriving (Eq, Show)

-- | A proof element consists of the sibling hash and a flag indicating
--   whether the sibling is on the left (True) or right (False) of the node.
type ProofElem = (Hash, Bool)

-- | A Merkle proof is a list of proof elements from leaf to root.
type MerkleProof = [ProofElem]
