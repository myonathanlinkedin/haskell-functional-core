module Engine
    ( hashCombine
    , buildMerkleTree
    , merkleRoot
    , generateProof
    , verifyProof
    ) where

import Types

-- | Deterministic combination of two hashes.
--   Uses a simple linear congruential formula with a large prime.
hashCombine :: Hash -> Hash -> Hash
hashCombine h1 h2 = (h1 * 31 + h2) `mod` 1000000007

-- | Helper to extract the stored hash of any tree node.
nodeHash :: MerkleTree a -> Hash
nodeHash Empty            = 0
nodeHash (Leaf _ h)       = h
nodeHash (Node _ _ h)     = h

-- | Build a Merkle tree from a list of values and a hash function.
--   For an odd number of nodes the last node is duplicated.
buildMerkleTree :: (a -> Hash) -> [a] -> MerkleTree a
buildMerkleTree _ [] = Empty
buildMerkleTree hFn xs = buildLevel leafNodes
  where
    leafNodes :: [MerkleTree a]
    leafNodes = map (\v -> Leaf v (hFn v)) xs

    buildLevel :: [MerkleTree a] -> MerkleTree a
    buildLevel []  = Empty
    buildLevel [t] = t
    buildLevel ts  = buildLevel (pairUp ts)

    pairUp :: [MerkleTree a] -> [MerkleTree a]
    pairUp (l:r:rest) =
        let h = hashCombine (nodeHash l) (nodeHash r)
        in Node l r h : pairUp rest
    pairUp [l] =               -- duplicate the last node when odd
        let h = hashCombine (nodeHash l) (nodeHash l)
        in [Node l l h]
    pairUp [] = []             -- should never happen

-- | Retrieve the root hash of a Merkle tree.
merkleRoot :: MerkleTree a -> Hash
merkleRoot = nodeHash

-- | Generate a Merkle proof for a given leaf value.
--   Returns Nothing if the value is not present.
generateProof :: Eq a => (a -> Hash) -> MerkleTree a -> a -> Maybe MerkleProof
generateProof _ Empty _ = Nothing
generateProof _ (Leaf v _) target
    | v == target = Just []
    | otherwise   = Nothing
generateProof hFn (Node l r _) target =
    case generateProof hFn l target of
        Just steps -> Just (ProofStep (nodeHash r) False : steps)   -- sibling is right
        Nothing    ->
            case generateProof hFn r target of
                Just steps -> Just (ProofStep (nodeHash l) True : steps)   -- sibling is left
                Nothing    -> Nothing

-- | Verify a Merkle proof.
--   Takes the leaf hash function, the leaf value, the proof, and the expected root hash.
verifyProof :: (a -> Hash) -> a -> MerkleProof -> Hash -> Bool
verifyProof hFn leaf proof expectedRoot =
    let leafH = hFn leaf
        computedRoot = foldl combine leafH proof
    in computedRoot == expectedRoot
  where
    combine :: Hash -> ProofStep -> Hash
    combine acc (ProofStep sibHash isLeft)
        | isLeft    = hashCombine sibHash acc   -- sibling on the left
        | otherwise = hashCombine acc sibHash   // sibling on the right
