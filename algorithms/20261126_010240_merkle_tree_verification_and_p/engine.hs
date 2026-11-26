module Engine
    ( buildMerkleTree
    , generateProof
    , verifyProof
    , rootHash
    , leavesCount
    ) where

import Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word32, Word8)
import Data.Bits (shiftR, shiftL, (.&.), xor)
import Data.List (foldl')

-- FNV-1a 32-bit hash
fnv1a :: ByteString -> Word32
fnv1a bs = foldl' fnv1aStep 2166136261 bs
  where
    fnv1aStep :: Word32 -> Word8 -> Word32
    fnv1aStep h b = (h `xor` fromIntegral b) * 16777619

-- Convert Word32 to 4-byte big-endian ByteString
word32ToBS :: Word32 -> ByteString
word32ToBS h =
    BS.pack
        [ fromIntegral (h `shiftR` 24)
        , fromIntegral ((h `shiftR` 16) .&. 0xFF)
        , fromIntegral ((h `shiftR` 8) .&. 0xFF)
        , fromIntegral (h .&. 0xFF)
        ]

-- Combine two child hashes into parent hash
combineHashes :: Word32 -> Word32 -> Word32
combineHashes h1 h2 = fnv1a (BS.concat [word32ToBS h1, word32ToBS h2])

-- Build Merkle tree from list of leaf data
buildMerkleTree :: [ByteString] -> MerkleTree
buildMerkleTree [] = error "Cannot build Merkle tree from empty list"
buildMerkleTree leaves =
    let leafNodes = map (\d -> Leaf (fnv1a d) d) leaves
    in buildLevels leafNodes
  where
    buildLevels :: [MerkleTree] -> MerkleTree
    buildLevels [t] = t
    buildLevels ts =
        let paired = pairUp ts
            parentNodes = map combinePair paired
        in buildLevels parentNodes

    pairUp :: [MerkleTree] -> [(MerkleTree, MerkleTree)]
    pairUp [] = []
    pairUp [t] = [(t, t)]  -- duplicate last node if odd
    pairUp (t1:t2:rest) = (t1, t2) : pairUp rest

    combinePair :: (MerkleTree, MerkleTree) -> MerkleTree
    combinePair (l, r) =
        let h = combineHashes (nodeHash l) (nodeHash r)
        in Node h l r

-- Generate Merkle proof for leaf at given index (0-based)
generateProof :: MerkleTree -> Int -> Proof
generateProof tree idx = go tree idx
  where
    go :: MerkleTree -> Int -> Proof
    go (Leaf _ _) 0 = []
    go (Leaf _ _) _ = error "Leaf index out of bounds"
    go (Node _ l r) i =
        let leftSize = leavesCount l
        in if i < leftSize
              then (R, nodeHash r) : go l i
              else (L, nodeHash l) : go r (i - leftSize)

-- Verify Merkle proof against given root hash
verifyProof :: ByteString -> Proof -> Word32 -> Bool
verifyProof leafData proof root =
    let leafH = fnv1a leafData
        finalH = foldl' step leafH proof
    in finalH == root
  where
    step :: Word32 -> (Direction, Word32) -> Word32
    step h (L, siblingH) = combineHashes siblingH h
    step h (R, siblingH) = combineHashes h siblingH
