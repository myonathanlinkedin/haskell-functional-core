module Core (
    MerkleTree,
    buildMerkleTree,
    rootHash,
    generateProof,
    verifyProof,
    hashLeaf,
    hashNode
) where

import Data.Bits (shiftL, xor)
import Data.List (foldl')
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

type Hash = String

-- Simple deterministic hash (not cryptographic)
simpleHash :: String -> Hash
simpleHash = show . foldl' (\h c -> (h `shiftL` 5) `xor` fromEnum c) 0

hashLeaf :: BS.ByteString -> Hash
hashLeaf = simpleHash . BSC.unpack

hashNode :: Hash -> Hash -> Hash
hashNode l r = simpleHash (l ++ r)

data MerkleTree a
    = Empty
    | Leaf Hash a
    | Node Hash (MerkleTree a) (MerkleTree a)
    deriving (Show, Eq)

nodeHash :: MerkleTree a -> Hash
nodeHash Empty          = ""
nodeHash (Leaf h _)    = h
nodeHash (Node h _ _)  = h

buildMerkleTree :: [a] -> (a -> BS.ByteString) -> MerkleTree a
buildMerkleTree [] _ = Empty
buildMerkleTree xs leafToBS = build (map leafNode xs)
  where
    leafNode x = Leaf (hashLeaf (leafToBS x)) x
    build [t] = t
    build ts  = build (pairUp ts)
    pairUp (l:r:rest) = Node (hashNode (nodeHash l) (nodeHash r)) l r : pairUp rest
    pairUp [t]        = [Node (hashNode (nodeHash t) (nodeHash t)) t t]  -- duplicate if odd
    pairUp []         = []

rootHash :: MerkleTree a -> Maybe Hash
rootHash Empty        = Nothing
rootHash (Leaf h _)   = Just h
rootHash (Node h _ _) = Just h

-- Proof is a list of (sibling hash, sibling is left?)
generateProof :: (Eq a) => MerkleTree a -> a -> Maybe [(Hash, Bool)]
generateProof tree target = go tree
  where
    go Empty = Nothing
    go (Leaf _ v)
        | v == target = Just []
        | otherwise   = Nothing
    go (Node _ l r) =
        case go l of
            Just p  -> Just ((nodeHash r, False) : p)  -- sibling on right
            Nothing -> case go r of
                Just p  -> Just ((nodeHash l, True) : p)   -- sibling on left
                Nothing -> Nothing

verifyProof :: Hash -> a -> (a -> BS.ByteString) -> [(Hash, Bool)] -> Bool
verifyProof root leaf leafToBS proof = computedRoot == root
  where
    leafH = hashLeaf (leafToBS leaf)
    computedRoot = foldl' combine leafH proof
    combine h (siblingHash, isLeft) =
        if isLeft then hashNode siblingHash h else hashNode h siblingHash
