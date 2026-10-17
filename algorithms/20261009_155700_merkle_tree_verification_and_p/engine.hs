module Engine ( buildMerkleTree
              , treeRoot
              , generateProof
              , verifyProof
              , hashLeaf
              ) where

import Types
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Bits (xor, shiftR, (.&.))
import Data.Word (Word64)
import Data.List (foldl')

--------------------------------------------------------------------
-- Simple deterministic 64‑bit FNV‑1a hash (pure, no external libs)
--------------------------------------------------------------------
fnvOffsetBasis :: Word64
fnvOffsetBasis = 14695981039346656037

fnvPrime :: Word64
fnvPrime = 1099511628211

hashBytes :: ByteString -> Hash
hashBytes bs = word64ToBS $ BS.foldl' fnvStep fnvOffsetBasis bs
  where
    fnvStep :: Word64 -> Word8 -> Word64
    fnvStep h b = (h `xor` fromIntegral b) * fnvPrime

-- Convert a Word64 into an 8‑byte little‑endian ByteString
word64ToBS :: Word64 -> ByteString
word64ToBS w = BS.pack [ fromIntegral ((w `shiftR` (8*i)) .&. 0xff) | i <- [0..7] ]

--------------------------------------------------------------------
-- Public hash helpers
--------------------------------------------------------------------
hashLeaf :: ByteString -> Hash
hashLeaf = hashBytes

hashCombine :: Hash -> Hash -> Hash
hashCombine h1 h2 = hashBytes (BS.append h1 h2)

--------------------------------------------------------------------
-- Merkle tree construction
--------------------------------------------------------------------
buildMerkleTree :: [ByteString] -> MerkleTree
buildMerkleTree [] = error "Cannot build a Merkle tree from an empty list"
buildMerkleTree xs = buildFromLeaves leafNodes
  where
    leafNodes :: [MerkleTree]
    leafNodes = map (Leaf . hashLeaf) xs

    buildFromLeaves :: [MerkleTree] -> MerkleTree
    buildFromLeaves [t] = t
    buildFromLeaves ts  = buildFromLeaves (pairUp ts)

    pairUp :: [MerkleTree] -> [MerkleTree]
    pairUp [] = []
    pairUp [t] = [makeNode t t]               -- duplicate last when odd
    pairUp (l:r:rest) = makeNode l r : pairUp rest

    makeNode :: MerkleTree -> MerkleTree -> MerkleTree
    makeNode l r = Node { nodeHash = hashCombine (nodeHash' l) (nodeHash' r)
                        , leftChild = l
                        , rightChild = r }

    nodeHash' :: MerkleTree -> Hash
    nodeHash' (Leaf h)   = h
    nodeHash' (Node h _ _) = h

treeRoot :: MerkleTree -> Hash
treeRoot (Leaf h)   = h
treeRoot (Node h _ _) = h

--------------------------------------------------------------------
-- Proof generation
--------------------------------------------------------------------
generateProof :: MerkleTree -> Int -> Maybe MerkleProof
generateProof tree idx = go tree idx leafCount
  where
    leafCount = countLeaves tree

    go :: MerkleTree -> Int -> Int -> Maybe MerkleProof
    go (Leaf _) i n
        | i == 0 && n == 1 = Just []
        | otherwise        = Nothing
    go (Node _ l r) i n = 
        let half = n `div` 2
            (targetSide, siblingHash, isLeftSibling, newIdx, newSize) =
                if i < half
                then (l, nodeHash' r, False, i, half)   -- sibling on right
                else (r, nodeHash' l, True , i - half, n - half) -- sibling on left
        in case go targetSide newIdx newSize of
            Just rest -> Just ((siblingHash, isLeftSibling) : rest)
            Nothing   -> Nothing

    nodeHash' :: MerkleTree -> Hash
    nodeHash' (Leaf h)   = h
    nodeHash' (Node h _ _) = h

    countLeaves :: MerkleTree -> Int
    countLeaves (Leaf _) = 1
    countLeaves (Node _ l r) = countLeaves l + countLeaves r

--------------------------------------------------------------------
-- Proof verification
--------------------------------------------------------------------
verifyProof :: Hash -> MerkleProof -> Hash -> Bool
verifyProof leafHash proof expectedRoot = computedRoot == expectedRoot
  where
    computedRoot = foldl' step leafHash proof

    step :: Hash -> ProofElem -> Hash
    step curHash (siblingHash, isLeftSibling) =
        if isLeftSibling
            then hashCombine siblingHash curHash   -- sibling is left
            else hashCombine curHash siblingHash   -- sibling is right
