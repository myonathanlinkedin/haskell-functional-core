module Types where

-- | A binary digit used in Huffman codes.
data Bit = Zero | One deriving (Eq, Show)

-- | Huffman tree storing either a leaf with a symbol and its frequency,
--   or an internal node with combined weight and two subtrees.
data HuffmanTree a
    = Leaf a Int
    | Node Int (HuffmanTree a) (HuffmanTree a)
    deriving (Show)

-- | Retrieve the total weight (frequency) of a tree.
weight :: HuffmanTree a -> Int
weight (Leaf _ w)   = w
weight (Node w _ _) = w
