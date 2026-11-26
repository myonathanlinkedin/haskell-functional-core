module Types where

import Data.ByteString (ByteString)
import Data.Word (Word32)

data Direction = L | R deriving (Eq, Show)

type Proof = [(Direction, Word32)]

data MerkleTree
    = Leaf { leafHash :: Word32, leafData :: ByteString }
    | Node { nodeHash :: Word32, left :: MerkleTree, right :: MerkleTree }
    deriving (Show)

-- Utility functions
leavesCount :: MerkleTree -> Int
leavesCount (Leaf _ _) = 1
leavesCount (Node _ l r) = leavesCount l + leavesCount r

rootHash :: MerkleTree -> Word32
rootHash (Leaf h _) = h
rootHash (Node h _ _) = h
