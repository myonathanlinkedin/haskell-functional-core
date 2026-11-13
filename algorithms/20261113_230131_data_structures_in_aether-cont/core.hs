module Core (
    AetherMap,
    empty,
    singleton,
    insert,
    amLookup,
    delete,
    fromList,
    toList
) where

-- | A pure binary search tree keyed by a 2‑dimensional coordinate.
--   The ordering is lexicographic: (x1,y1) < (x2,y2) iff
--   x1 < x2 || (x1 == x2 && y1 < y2).
data AetherMap a
    = Empty
    | Node {
        key   :: (Int, Int),
        value :: a,
        left  :: AetherMap a,
        right :: AetherMap a
      }
    deriving (Show, Eq)

-- | The empty map.
empty :: AetherMap a
empty = Empty

-- | Construct a map with a single element.
singleton :: (Int, Int) -> a -> AetherMap a
singleton k v = Node k v Empty Empty

-- | Lexicographic comparison of coordinates.
compareCoord :: (Int, Int) -> (Int, Int) -> Ordering
compareCoord (x1, y1) (x2, y2) =
    case compare x1 x2 of
        EQ -> compare y1 y2
        ord -> ord

-- | Insert a key/value pair, replacing any existing value for the key.
insert :: (Int, Int) -> a -> AetherMap a -> AetherMap a
insert k v Empty = Node k v Empty Empty
insert k v t@(Node k' v' l r) =
    case compareCoord k k' of
        LT -> Node k' v' (insert k v l) r
        GT -> Node k' v' l (insert k v r)
        EQ -> Node k v l r   -- replace

-- | Lookup a value by key. Returns Nothing if the key is absent.
amLookup :: (Int, Int) -> AetherMap a -> Maybe a
amLookup _ Empty = Nothing
amLookup k (Node k' v' l r) =
    case compareCoord k k' of
        LT -> amLookup k l
        GT -> amLookup k r
        EQ -> Just v'

-- | Delete a key from the map. If the key is absent, returns the original map.
delete :: (Int, Int) -> AetherMap a -> AetherMap a
delete _ Empty = Empty
delete k (Node k' v' l r) =
    case compareCoord k k' of
        LT -> Node k' v' (delete k l) r
        GT -> Node k' v' l (delete k r)
        EQ -> glue l r
  where
    -- Join two BSTs preserving the ordering invariant.
    glue :: AetherMap a -> AetherMap a -> AetherMap a
    glue Empty t = t
    glue t Empty = t
    glue t1 t2   = let (kmin, vmin, t2') = deleteMin t2
                   in Node kmin vmin t1 t2'

    -- Remove the minimum element from a BST, returning its key/value and the new tree.
    deleteMin :: AetherMap a -> ((Int, Int), a, AetherMap a)
    deleteMin (Node k v Empty r) = (k, v, r)
    deleteMin (Node k v l r) =
        let (kmin, vmin, l') = deleteMin l
        in (kmin, vmin, Node k v l' r)
    deleteMin Empty = error "deleteMin on empty tree (should not happen)"

-- | Build a map from a list of key/value pairs.
--   Later occurrences of the same key overwrite earlier ones.
fromList :: [((Int, Int), a)] -> AetherMap a
fromList = foldr (uncurry insert) empty . reverse
  where
    uncurry f (k, v) = f k v

-- | Convert a map to an ascending list of key/value pairs.
toList :: AetherMap a -> [((Int, Int), a)]
toList Empty = []
toList (Node k v l r) = toList l ++ [(k, v)] ++ toList r
