module Types where

import qualified Data.Map.Strict as Map

-- | Unique identifier for a snapshot.
newtype SnapshotId = SnapshotId Int
  deriving (Eq, Ord, Show)

-- | The core data structure representing a versioned key‑value store.
--   Internally it keeps a map from keys to values and a history of snapshots.
data Mold k v = Mold
  { current   :: Map.Map k v               -- ^ Current key‑value map.
  , history   :: Map.Map SnapshotId (Map.Map k v) -- ^ Snapshots indexed by id.
  , nextId    :: Int                       -- ^ Counter for generating fresh SnapshotIds.
  } deriving (Show)

-- | Create an empty Mold.
emptyMold :: Mold k v
emptyMold = Mold
  { current = Map.empty
  , history = Map.empty
  , nextId  = 0
  }

-- | Result type for operations that may fail.
data MoldResult a
  = Success a
  | Failure String
  deriving (Show)

-- | Helper to extract a value or raise an error in IO.
unwrapResult :: MoldResult a -> IO a
unwrapResult (Success x) = pure x
unwrapResult (Failure msg) = error msg
