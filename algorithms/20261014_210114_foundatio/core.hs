module Heap (
    Heap,
    empty,
    singleton,
    insert,
    merge,
    findMin,
    deleteMin,
    fromList,
    toList,
    isValid
) where

-- | A leftist min‑heap.  The stored rank is the length of the shortest
--   path to an empty node (also called the null‑path length).
data Heap a = Empty
            | Node !Int a (Heap a) (Heap a)   -- ^ rank, element, left, right
            deriving (Show, Eq)

-- | The empty heap.
empty :: Heap a
empty = Empty

-- | Create a heap with a single element.
singleton :: a -> Heap a
singleton x = Node 1 x Empty Empty

-- | Retrieve the rank of a heap.
rank :: Heap a -> Int
rank Empty            = 0
rank (Node r _ _ _)   = r

-- | Construct a node while preserving the leftist property.
makeNode :: a -> Heap a -> Heap a -> Heap a
makeNode x a b
    | rank a >= rank b = Node (rank b + 1) x a b
    | otherwise        = Node (rank a + 1) x b a

-- | Merge two heaps.  Complexity: O(log n).
merge :: Ord a => Heap a -> Heap a -> Heap a
merge h Empty = h
merge Empty h = h
merge h1@(Node _ x a1 b1) h2@(Node _ y a2 b2)
    | x <= y    = makeNode x a1 (merge b1 h2)
    | otherwise = makeNode y a2 (merge h1 b2)

-- | Insert an element into a heap.
insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge (singleton x) h

-- | Retrieve the minimum element, if any.
findMin :: Heap a -> Maybe a
findMin Empty          = Nothing
findMin (Node _ x _ _) = Just x

-- | Delete the minimum element.
deleteMin :: Ord a => Heap a -> Heap a
deleteMin Empty          = Empty
deleteMin (Node _ _ l r) = merge l r

-- | Build a heap from a list.
fromList :: Ord a => [a] -> Heap a
fromList = foldr insert empty

-- | Extract the elements in non‑decreasing order.
toList :: Ord a => Heap a -> [a]
toList h = case findMin h of
    Nothing -> []
    Just x  -> x : toList (deleteMin h)

-- | Verify the heap invariant:
--   * The element at each node is ≤ its children.
--   * The leftist property (rank left ≥ rank right) holds.
isValid :: Ord a => Heap a -> Bool
isValid Empty = True
isValid (Node _ x l r) =
    let leftOk  = case l of
            Empty          -> True
            Node _ lx _ _  -> x <= lx
        rightOk = case r of
            Empty          -> True
            Node _ rx _ _  -> x <= rx
    in leftOk && rightOk && rank l >= rank r && isValid l && isValid r
