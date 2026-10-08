module Core (
    MerkleTree,
    buildMerkleTree,
    rootHash,
    merkleProof,
    verifyProof
) where

-- Simple deterministic hash function (placeholder)
hash :: String -> String
hash s = show (sum (map fromEnum s) `mod` 1000000)

leafHash :: String -> String
leafHash = hash

nodeHash :: String -> String -> String
nodeHash l r = hash (l ++ r)

data MerkleTree
    = Leaf String String               -- value, hash
    | Node String MerkleTree MerkleTree -- hash, left, right
    deriving (Show, Eq)

-- Build leaf nodes from values
buildLeaves :: [String] -> [MerkleTree]
buildLeaves = map (\v -> Leaf v (leafHash v))

-- Helper to extract stored hash
nodeOrLeafHash :: MerkleTree -> String
nodeOrLeafHash (Leaf _ h)   = h
nodeOrLeafHash (Node h _ _) = h

-- Pair up nodes, duplicating the last when necessary
pairUp :: [MerkleTree] -> [MerkleTree]
pairUp []       = []
pairUp [x]      = [x]  -- odd count: propagate the lone node upward
pairUp (x:y:xs) =
    let h = nodeHash (nodeOrLeafHash x) (nodeOrLeafHash y)
        n = Node h x y
    in n : pairUp xs

-- Recursively build the tree until a single root remains
buildTree :: [MerkleTree] -> MerkleTree
buildTree []  = error "Cannot build a Merkle tree from an empty list"
buildTree [t] = t
buildTree ts  = buildTree (pairUp ts)

-- Public interface: construct a Merkle tree from leaf values
buildMerkleTree :: [String] -> MerkleTree
buildMerkleTree = buildTree . buildLeaves

-- Retrieve the root hash
rootHash :: MerkleTree -> String
rootHash (Leaf _ h)   = h
rootHash (Node h _ _) = h

-- Count leaves in a subtree
leafCount :: MerkleTree -> Int
leafCount (Leaf _ _)   = 1
leafCount (Node _ l r) = leafCount l + leafCount r

-- Proof element: sibling hash and a flag indicating if sibling is on the left
type Proof = [(String, Bool)]  -- (siblingHash, isLeftSibling)

-- Generate a Merkle proof for the leaf at the given index (0‑based)
merkleProof :: MerkleTree -> Int -> Maybe Proof
merkleProof tree idx = go tree idx (leafCount tree)
  where
    go (Leaf _ _) 0 _ = Just []
    go (Leaf _ _) _ _ = Nothing
    go (Node _ l r) i totalLeaves =
        let leftLeaves = leafCount l
        in if i < leftLeaves
           then case go l i leftLeaves of
                Just p  -> Just ((nodeOrLeafHash r, False) : p) -- sibling on right
                Nothing -> Nothing
           else case go r (i - leftLeaves) (totalLeaves - leftLeaves) of
                Just p  -> Just ((nodeOrLeafHash l, True) : p)  -- sibling on left
                Nothing -> Nothing

-- Verify a proof against a claimed leaf value and expected root hash
verifyProof :: String -> Proof -> String -> Bool
verifyProof leafVal proof expectedRoot =
    let start = leafHash leafVal
        recomputed = foldl step start proof
    in recomputed == expectedRoot
  where
    step cur (siblingHash, isLeft) =
        if isLeft
        then nodeHash siblingHash cur
        else nodeHash cur siblingHash
