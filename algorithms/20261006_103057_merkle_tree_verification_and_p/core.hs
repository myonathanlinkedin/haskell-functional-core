module MerkleTree where

import Data.List (foldl')

-- | Merkle tree data type. Each node stores its hash.
data MerkleTree a = Leaf Int a
                  | Node Int (MerkleTree a) (MerkleTree a)
                  deriving (Show, Eq)

-- | Simple deterministic hash function for strings.
hashData :: String -> Int
hashData = foldl' (\h c -> h * 31 + fromEnum c) 0

-- | Combine two child hashes into a parent hash.
hashCombine :: Int -> Int -> Int
hashCombine h1 h2 = h1 * 131 + h2

-- | Compute the hash stored at a node.
nodeHash :: MerkleTree a -> Int
nodeHash (Leaf h _)   = h
nodeHash (Node h _ _) = h

-- | Number of leaves in the tree.
size :: MerkleTree a -> Int
size (Leaf _ _)   = 1
size (Node _ l r) = size l + size r

-- | Build a Merkle tree from a list of strings.
buildMerkleTree :: [String] -> MerkleTree String
buildMerkleTree [] = Leaf 0 ""
buildMerkleTree xs = build (map (\s -> Leaf (hashData s) s) xs)
  where
    build [t] = t
    build ts  = build (combine ts)

    combine :: [MerkleTree String] -> [MerkleTree String]
    combine [] = []
    combine [t] = [t]
    combine (t1:t2:rest) =
      let h = hashCombine (nodeHash t1) (nodeHash t2)
      in Node h t1 t2 : combine rest

-- | Retrieve the root hash of the tree.
rootHash :: MerkleTree a -> Int
rootHash = nodeHash

-- | Generate a Merkle proof for the leaf at the given index.
--   The proof is a list of (isLeftSibling, siblingHash) pairs,
--   where isLeftSibling is True if the sibling is on the left.
proof :: MerkleTree String -> Int -> [(Bool, Int)]
proof tree idx
  | idx < 0 || idx >= size tree = error "Index out of bounds"
  | otherwise = reverse (go tree idx [])
  where
    go :: MerkleTree String -> Int -> [(Bool, Int)] -> [(Bool, Int)]
    go (Leaf _ _) _ acc = acc
    go (Node _ l r) i acc
      | i < leftSize = go l i ((False, nodeHash r) : acc)
      | otherwise    = go r (i - leftSize) ((True, nodeHash l) : acc)
      where leftSize = size l

-- | Verify a Merkle proof.
--   Returns True if the computed root hash matches the given root hash.
verifyProof :: [(Bool, Int)] -> Int -> String -> Int -> Bool
verifyProof proofList idx leafValue root =
  let leafHash = hashData leafValue
      computed = foldl' combine leafHash proofList
  in computed == root
  where
    combine h (isLeft, siblingHash)
      | isLeft    = hashCombine siblingHash h
      | otherwise = hashCombine h siblingHash
