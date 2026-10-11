module Types where

-- | Simple deterministic hash type.
type Hash = Int

-- | Merkle tree data structure.
data MerkleTree a
    = Empty
    | Leaf { leafValue :: a, leafHash :: Hash }
    | Node { leftChild  :: MerkleTree a
           , rightChild :: MerkleTree a
           , nodeHash   :: Hash
           }
    deriving (Show, Eq)

-- | One step of a Merkle proof.
--   'siblingHash' is the hash of the sibling node.
--   'siblingIsLeft' is True when the sibling is the left child,
--   False when it is the right child.
data ProofStep = ProofStep
    { siblingHash     :: Hash
    , siblingIsLeft   :: Bool
    } deriving (Show, Eq)

-- | A Merkle proof is a list of steps from leaf up to the root.
type MerkleProof = [ProofStep]
