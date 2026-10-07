module Types where

import Data.IORef
import Control.Concurrent.MVar

-- | A node in the optimistic lock‑coupled linked list.
data Node a = Node
    { val      :: a                 -- ^ Stored value (sentinel has undefined)
    , nextRef  :: IORef (Maybe (Node a)) -- ^ Reference to the next node
    , lock     :: MVar ()           -- ^ Exclusive lock for updates
    , ver      :: IORef Int         -- ^ Version counter for validation
    }

-- | Helper to create a fresh node.
newNode :: a -> Maybe (Node a) -> IO (Node a)
newNode v nxt = do
    nxtRef <- newIORef nxt
    lk     <- newMVar ()
    vr     <- newIORef 0
    return $ Node v nxtRef lk vr
