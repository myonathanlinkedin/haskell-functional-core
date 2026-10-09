module Engine where

import Types
import Data.List (sortBy)
import Data.Ord (comparing)

-- | Build a Huffman tree from a list of (symbol, frequency) pairs.
--   The list must contain at least one element.
buildTree :: Ord a => [(a, Int)] -> HuffmanTree a
buildTree [] = error "Cannot build Huffman tree from empty list"
buildTree xs = combine initialLeaves
  where
    -- initial list of leaf nodes sorted by weight
    initialLeaves = sortBy (comparing weight) [Leaf sym freq | (sym, freq) <- xs]

    -- repeatedly combine the two smallest trees until one remains
    combine [t] = t
    combine (t1:t2:ts) =
        let merged = Node (weight t1 + weight t2) t1 t2
        in combine $ insertByWeight merged ts
    combine _ = error "Unexpected pattern in combine"

    -- insert a tree into a list keeping the list sorted by weight
    insertByWeight :: HuffmanTree a -> [HuffmanTree a] -> [HuffmanTree a]
    insertByWeight t [] = [t]
    insertByWeight t (y:ys)
        | weight t <= weight y = t : y : ys
        | otherwise            = y : insertByWeight t ys

-- | Generate a code table mapping each symbol to its Huffman bit sequence.
generateCodes :: HuffmanTree a -> [(a, [Bit])]
generateCodes tree = go [] tree
  where
    go :: [Bit] -> HuffmanTree a -> [(a, [Bit])]
    go prefix (Leaf sym _) = [(sym, reverse prefix)]
    go prefix (Node _ left right) =
        go (Zero:prefix) left ++ go (One:prefix) right

-- | Encode a list of symbols using a pre‑computed code table.
encode :: (Eq a) => [(a, [Bit])] -> [a] -> [Bit]
encode codeTable = concatMap lookupCode
  where
    lookupCode sym =
        case lookup sym codeTable of
            Just bits -> bits
            Nothing   -> error $ "Symbol not found in code table: " ++ show sym

-- | Decode a sequence of bits using the Huffman tree.
decode :: HuffmanTree a -> [Bit] -> [a]
decode tree bits = decodeFrom tree bits
  where
    decodeFrom _ [] = []
    decodeFrom (Leaf sym _) rest = sym : decodeFrom tree rest
    decodeFrom (Node _ left right) (b:bs) =
        case b of
            Zero -> decodeFrom left bs
            One  -> decodeFrom right bs
    decodeFrom (Leaf _ _) _ = error "Invalid state: reached leaf with remaining bits"

-- | Helper to compute symbol frequencies in a list.
frequencies :: (Ord a) => [a] -> [(a, Int)]
frequencies xs = map (\g -> (head g, length g)) . groupSort $ xs
  where
    groupSort = group . sortBy compare
    group [] = []
    group (y:ys) = let (eq, rest) = span (== y) ys
                   in (y:eq) : group rest
