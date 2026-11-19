module Core (Heap, empty, isEmpty, singleton, insert, merge, findMin, deleteMin, heapSort, isValid) where

-- Leftist Heap implementation (min-heap)
data Heap a = Empty
            | Node { rank :: Int      -- distance to nearest empty leaf
                   , value :: a
                   , left  :: Heap a
                   , right :: Heap a
                   } deriving (Show, Eq)

-- Public constructors
empty :: Heap a
empty = Empty

isEmpty :: Heap a -> Bool
isEmpty Empty = True
isEmpty _     = False

singleton :: a -> Heap a
singleton x = Node 1 x Empty Empty

-- Internal helper: maintain leftist property after merging two sub‑heaps
makeNode :: Ord a => a -> Heap a -> Heap a -> Heap a
makeNode v a b
  | rank a >= rank b = Node (rank b + 1) v a b
  | otherwise        = Node (rank a + 1) v b a

-- Merge two heaps (O(log n))
merge :: Ord a => Heap a -> Heap a -> Heap a
merge h Empty = h
merge Empty h = h
merge h1@(Node _ v1 l1 r1) h2@(Node _ v2 l2 r2)
  | v1 <= v2  = makeNode v1 l1 (merge r1 h2)
  | otherwise = makeNode v2 l2 (merge h1 r2)

-- Insert an element (O(log n))
insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge (singleton x) h

-- Find minimum element (O(1))
findMin :: Ord a => Heap a -> Maybe a
findMin Empty          = Nothing
findMin (Node _ v _ _) = Just v

-- Delete minimum element (O(log n))
deleteMin :: Ord a => Heap a -> Heap a
deleteMin Empty          = Empty
deleteMin (Node _ _ l r) = merge l r

-- Convert a list to a heap (O(n))
fromList :: Ord a => [a] -> Heap a
fromList = foldr insert empty

-- Heap sort using the leftist heap (O(n log n))
heapSort :: Ord a => [a] -> [a]
heapSort = extract . fromList
  where
    extract h = case findMin h of
                  Nothing -> []
                  Just v  -> v : extract (deleteMin h)

-- Validation utilities -------------------------------------------------------

-- Check the leftist property and heap ordering invariant
isValid :: Ord a => Heap a -> Bool
isValid = go
  where
    go Empty = True
    go (Node r v l rgt) =
      rank l >= rank rgt &&                -- leftist property
      (case l of
         Empty -> True
         Node _ lv _ _ -> v <= lv) &&
      (case rgt of
         Empty -> True
         Node _ rv _ _ -> v <= rv) &&
      go l && go rgt
