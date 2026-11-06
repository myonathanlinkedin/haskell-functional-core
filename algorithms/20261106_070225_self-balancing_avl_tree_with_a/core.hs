module AVL ( AVL
           , empty
           , insert
           , delete
           , lookup
           , fromList
           , toList
           , isBalanced
           ) where

-- Pure AVL tree implementation
data AVL k v = Empty
             | Node { key    :: !k
                    , value  :: v
                    , left   :: AVL k v
                    , right  :: AVL k v
                    , height :: !Int
                    } deriving (Show, Eq)

empty :: AVL k v
empty = Empty

-- Height helper
nodeHeight :: AVL k v -> Int
nodeHeight Empty                = 0
nodeHeight (Node _ _ _ _ h)    = h

mkNode :: k -> v -> AVL k v -> AVL k v -> AVL k v
mkNode k v l r = Node k v l r h
  where h = 1 + max (nodeHeight l) (nodeHeight r)

balanceFactor :: AVL k v -> Int
balanceFactor Empty                = 0
balanceFactor (Node _ _ l r _)    = nodeHeight l - nodeHeight r

rotateRight :: AVL k v -> AVL k v
rotateRight (Node k v (Node lk lv ll lr _) r _) =
    mkNode lk lv ll (mkNode k v lr r)
rotateRight t = t  -- should not happen

rotateLeft :: AVL k v -> AVL k v
rotateLeft (Node k v l (Node rk rv rl rr _) _) =
    mkNode rk rv (mkNode k v l rl) rr
rotateLeft t = t  -- should not happen

rebalance :: AVL k v -> AVL k v
rebalance t@(Node k v l r _)
    | bf > 1 && balanceFactor l >= 0 = rotateRight t
    | bf > 1 && balanceFactor l < 0  = rotateRight (mkNode k v (rotateLeft l) r)
    | bf < -1 && balanceFactor r <= 0 = rotateLeft t
    | bf < -1 && balanceFactor r > 0  = rotateLeft (mkNode k v l (rotateRight r))
    | otherwise = mkNode k v l r
  where bf = balanceFactor t
rebalance t = t

insert :: (Ord k) => k -> v -> AVL k v -> AVL k v
insert k v Empty = Node k v Empty Empty 1
insert k v n@(Node nk nv nl nr _)
    | k < nk   = rebalance $ mkNode nk nv (insert k v nl) nr
    | k > nk   = rebalance $ mkNode nk nv nl (insert k v nr)
    | otherwise = mkNode nk v nl nr  -- replace value

lookup :: (Ord k) => k -> AVL k v -> Maybe v
lookup _ Empty = Nothing
lookup k (Node nk nv nl nr _)
    | k < nk   = lookup k nl
    | k > nk   = lookup k nr
    | otherwise = Just nv

-- Find the node with minimum key
findMin :: AVL k v -> (k, v)
findMin (Node k v Empty _ _) = (k, v)
findMin (Node _ _ l _ _)     = findMin l
findMin Empty                = error "findMin on empty tree"

delete :: (Ord k) => k -> AVL k v -> AVL k v
delete _ Empty = Empty
delete k n@(Node nk nv nl nr _)
    | k < nk   = rebalance $ mkNode nk nv (delete k nl) nr
    | k > nk   = rebalance $ mkNode nk nv nl (delete k nr)
    | otherwise = case (nl, nr) of
        (Empty, _) -> nr
        (_, Empty) -> nl
        _ -> let (succK, succV) = findMin nr
                 newRight = delete succK nr
             in rebalance $ mkNode succK succV nl newRight

fromList :: (Ord k) => [(k,v)] -> AVL k v
fromList = foldr (uncurry insert) empty

toList :: AVL k v -> [(k,v)]
toList Empty = []
toList (Node k v l r _) = toList l ++ [(k,v)] ++ toList r

-- Verification that every node satisfies AVL balance invariant
isBalanced :: AVL k v -> Bool
isBalanced Empty = True
isBalanced (Node _ _ l r _) =
    abs (balanceFactor (Node undefined undefined l r undefined)) <= 1
    && isBalanced l && isBalanced r
