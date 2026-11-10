module Core (Store, emptyStore, set, get, getAt, latestVersion, pruneOlderThan) where

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

-- | Persistent versioned key‑value store.
--   Each mutation creates a new immutable version.
data Store k v = Store
  { currentVersion :: Int                     -- ^ Latest version number
  , versionedMaps  :: IM.IntMap (M.Map k v)   -- ^ Mapping from version → map
  } deriving (Show, Eq)

-- | Initialise an empty store at version 0.
emptyStore :: Ord k => Store k v
emptyStore = Store 0 (IM.singleton 0 M.empty)

-- | Insert or update a key, producing a new version.
set :: Ord k => k -> v -> Store k v -> Store k v
set key val (Store ver vmaps) =
  let newVer   = ver + 1
      prevMap  = IM.findWithDefault M.empty ver vmaps
      newMap   = M.insert key val prevMap
   in Store newVer (IM.insert newVer newMap vmaps)

-- | Retrieve the value for a key from the latest version.
get :: Ord k => k -> Store k v -> Maybe v
get key (Store ver vmaps) = M.lookup key =<< IM.lookup ver vmaps

-- | Retrieve the value for a key from a specific version.
getAt :: Ord k => k -> Int -> Store k v -> Maybe v
getAt key v (Store _ vmaps) = M.lookup key =<< IM.lookup v vmaps

-- | Return the most recent version number.
latestVersion :: Store k v -> Int
latestVersion = currentVersion

-- | Discard all versions older than the supplied minimum version.
pruneOlderThan :: Int -> Store k v -> Store k v
pruneOlderThan minVer (Store ver vmaps) =
  Store ver (IM.filterWithKey (\k _ -> k >= minVer) vmaps)
