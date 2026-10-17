module Core (OLCMap, insert, delete, optimisticRead, validateRead, fromList, toList) where

-- | A node in an ordered linked map. The 'nVer' field acts as a version stamp.
data Node k v = Node
  { nKey   :: k
  , nVal   :: v
  , nNext  :: OLCMap k v
  , nVer   :: Int
  } deriving (Show, Eq)

-- | The map is a possibly empty linked list.
type OLCMap k v = Maybe (Node k v)

-- | Insert a key/value pair, preserving order.
--   The version of every node whose structural link changes is incremented.
insert :: Ord k => k -> v -> OLCMap k v -> OLCMap k v
insert k v Nothing = Just (Node k v Nothing 1)
insert k v (Just n)
  | k < nKey n = Just n { nNext = insert k v (nNext n), nVer = nVer n + 1 }
  | k == nKey n = Just n { nVal = v, nVer = nVer n + 1 }
  | otherwise   = Just n { nNext = insert k v (nNext n), nVer = nVer n + 1 }

-- | Delete a key, preserving order.
--   The version of every node whose structural link changes is incremented.
delete :: Ord k => k -> OLCMap k v -> OLCMap k v
delete _ Nothing = Nothing
delete k (Just n)
  | k < nKey n = Just n { nNext = delete k (nNext n), nVer = nVer n + 1 }
  | k == nKey n = case nNext n of
      Nothing   -> Nothing
      Just nxt  -> Just nxt { nVer = nVer nxt + 1 }   -- promote successor
  | otherwise   = Just n { nNext = delete k (nNext n), nVer = nVer n + 1 }

-- | Optimistic read: traverse the list, collecting version stamps.
--   Returns the value together with the list of visited versions (head‑to‑target order).
optimisticRead :: Ord k => k -> OLCMap k v -> Maybe (v, [Int])
optimisticRead k = go []
  where
    go _ Nothing = Nothing
    go acc (Just n)
      | k < nKey n = go (nVer n : acc) (nNext n)
      | k == nKey n = Just (nVal n, reverse (nVer n : acc))
      | otherwise   = go (nVer n : acc) (nNext n)

-- | Validate that the version list observed during an optimistic read
--   still matches the current structure.
validateRead :: Ord k => k -> OLCMap k v -> [Int] -> Bool
validateRead k m vs = case optimisticRead k m of
    Just (_, vs') -> vs' == vs
    Nothing       -> False

-- | Build a map from an association list (the list need not be sorted).
fromList :: Ord k => [(k,v)] -> OLCMap k v
fromList = foldr (uncurry insert) Nothing

-- | Convert the map back to a sorted association list.
toList :: OLCMap k v -> [(k,v)]
toList Nothing = []
toList (Just n) = (nKey n, nVal n) : toList (nNext n)
