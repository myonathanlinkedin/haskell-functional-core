module Engine ( insertMold
              , deleteMold
              , lookupMold
              , snapshotMold
              , revertMold
              , listKeys
              , listSnapshots
              ) where

import Types
import qualified Data.Map.Strict as Map

-- | Insert a key/value pair. Overwrites existing key.
insertMold :: (Ord k) => k -> v -> Mold k v -> MoldResult (Mold k v)
insertMold k v mold =
  Success mold { current = Map.insert k v (current mold) }

-- | Delete a key. Fails if the key does not exist.
deleteMold :: (Ord k) => k -> Mold k v -> MoldResult (Mold k v)
deleteMold k mold =
  case Map.lookup k (current mold) of
    Nothing -> Failure $ "deleteMold: key not found: " ++ show k
    Just _  -> Success mold { current = Map.delete k (current mold) }

-- | Lookup a key. Returns Failure if absent.
lookupMold :: (Ord k) => k -> Mold k v -> MoldResult v
lookupMold k mold =
  case Map.lookup k (current mold) of
    Nothing -> Failure $ "lookupMold: key not found: " ++ show k
    Just v  -> Success v

-- | Take a snapshot of the current state. Returns the new Mold and the SnapshotId.
snapshotMold :: Mold k v -> MoldResult (Mold k v, SnapshotId)
snapshotMold mold =
  let sid = SnapshotId (nextId mold)
      newHist = Map.insert sid (current mold) (history mold)
  in Success ( mold { history = newHist, nextId = nextId mold + 1 }, sid )

-- | Revert to a previously taken snapshot. Fails if the SnapshotId is unknown.
revertMold :: SnapshotId -> Mold k v -> MoldResult (Mold k v)
revertMold sid mold =
  case Map.lookup sid (history mold) of
    Nothing   -> Failure $ "revertMold: unknown snapshot " ++ show sid
    Just snap -> Success mold { current = snap }

-- | List all keys currently stored.
listKeys :: Mold k v -> [k]
listKeys = Map.keys . current

-- | List all snapshot identifiers in creation order.
listSnapshots :: Mold k v -> [SnapshotId]
listSnapshots = Map.keys . history
