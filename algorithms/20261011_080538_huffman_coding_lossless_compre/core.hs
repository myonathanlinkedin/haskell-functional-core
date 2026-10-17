module Core (
    HuffmanTree(..),
    frequencies,
    buildTree,
    generateCodes,
    encode,
    decode
) where

import Data.List (sortOn, insertBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map

-- | Huffman tree definition.
data HuffmanTree a
    = Empty
    | Leaf a Int               -- ^ Symbol and its weight
    | Node Int (HuffmanTree a) (HuffmanTree a)  -- ^ Weight and two subtrees
    deriving (Show, Eq)

-- | Retrieve the weight (frequency) of a tree node.
treeWeight :: HuffmanTree a -> Int
treeWeight Empty          = 0
treeWeight (Leaf _ w)    = w
treeWeight (Node w _ _)  = w

-- | Build a frequency table from a list of symbols.
frequencies :: (Ord a) => [a] -> [(a, Int)]
frequencies xs = Map.toList $ Map.fromListWith (+) [(x,1) | x <- xs]

-- | Build a Huffman tree from a frequency table.
buildTree :: (Ord a) => [(a, Int)] -> HuffmanTree a
buildTree [] = Empty
buildTree freqList = combine initialLeaves
  where
    -- initial list of leaf nodes sorted by weight
    initialLeaves = sortOn treeWeight [Leaf sym w | (sym,w) <- freqList]

    -- repeatedly combine two smallest trees
    combine :: [HuffmanTree a] -> HuffmanTree a
    combine []  = Empty
    combine [t] = t
    combine (t1:t2:ts) =
        let newNode = Node (treeWeight t1 + treeWeight t2) t1 t2
            ts' = insertBy (comparing treeWeight) newNode ts
        in combine ts'

-- | Generate a code table (symbol -> list of bits) from a Huffman tree.
generateCodes :: (Ord a) => HuffmanTree a -> [(a, [Bool])]
generateCodes tree = go tree []
  where
    go Empty _ = []
    go (Leaf sym _) path = [(sym, reverse path)]
    go (Node _ l r) path = go l (False:path) ++ go r (True:path)

-- | Encode a list of symbols into a list of bits using Huffman coding.
--   The frequency table is derived from the input if not supplied.
encode :: (Ord a) => [a] -> [Bool]
encode [] = []
encode xs =
    let freqTable = frequencies xs
        tree = buildTree freqTable
        codeMap = Map.fromList (generateCodes tree)
        encodeSym sym = case Map.lookup sym codeMap of
                          Just bits -> bits
                          Nothing   -> error "encode: symbol not in code map"
    in concatMap encodeSym xs

-- | Decode a list of bits into a list of symbols using the provided Huffman tree.
decode :: HuffmanTree a -> [Bool] -> [a]
decode Empty _ = []
decode tree bits = go tree bits
  where
    go _ [] = []
    go (Leaf sym _) rest = sym : go tree rest
    go (Node _ l r) (b:bs) = go (if not b then l else r) bs
    go _ [] = error "decode: incomplete bit sequence"
