module Engine (newList, contains, insert, delete) where

import Types
import Control.Concurrent.MVar (takeMVar, putMVar)
import Control.Exception (bracket_)
import Data.IORef
import Data.Maybe (isJust)

-- | Create an empty list with a sentinel head node.
newList :: Ord a => IO (Node a)
newList = newNode (error "sentinel") Nothing

-- | Acquire a node's lock.
lockNode :: Node a -> IO ()
lockNode n = takeMVar (lock n)

-- | Release a node's lock.
unlockNode :: Node a -> IO ()
unlockNode n = putMVar (lock n) ()

-- | Execute an action while holding a list of locks (in order).
withLocks :: [Node a] -> IO b -> IO b
withLocks [] act = act
withLocks (n:ns) act = bracket_ (lockNode n) (unlockNode n) (withLocks ns act)

-- | Read a node's version.
readVer :: Node a -> IO Int
readVer = readIORef . ver

-- | Increment a node's version.
incVer :: Node a -> IO ()
incVer n = modifyIORef' (ver n) (+1)

-- | Optimistic read of the list to check membership.
contains :: Ord a => Node a -> a -> IO Bool
contains headNode key = go =<< readIORef (nextRef headNode)
  where
    go :: Maybe (Node a) -> IO Bool
    go Nothing = return False
    go (Just cur) = do
        v1 <- readVer cur
        nxt <- readIORef (nextRef cur)
        v2 <- readVer cur
        if v1 /= v2
            then readIORef (nextRef headNode) >>= go   -- restart from head on conflict
            else case compare key (val cur) of
                EQ -> return True
                LT -> return False
                GT -> go nxt

-- | Find the predecessor and (maybe) current node where key should be.
findPos :: Ord a => Node a -> a -> IO (Node a, Maybe (Node a))
findPos pred key = do
    mcur <- readIORef (nextRef pred)
    case mcur of
        Nothing -> return (pred, Nothing)
        Just cur ->
            case compare key (val cur) of
                LT -> return (pred, Just cur)
                EQ -> return (pred, Just cur)
                GT -> findPos cur key

-- | Insert a key if not already present.
insert :: Ord a => Node a -> a -> IO ()
insert headNode key = go
  where
    go = do
        (pred, mcur) <- findPos headNode key
        let locks = case mcur of
                        Just cur -> [pred, cur]
                        Nothing  -> [pred]
        withLocks locks $ do
            -- validate that the structure hasn't changed
            predNext <- readIORef (nextRef pred)
            if predNext /= mcur
                then go   -- retry
                else case mcur of
                    Just cur -> do
                        curNext <- readIORef (nextRef cur)
                        curVer1 <- readVer cur
                        curVer2 <- readVer cur
                        if curVer1 /= curVer2
                            then go
                            else if val cur == key
                                then return ()   -- already present
                                else insertBetween pred cur
                    Nothing -> insertAtEnd pred
    insertBetween pred cur = do
        newN <- newNode key (Just cur)
        writeIORef (nextRef pred) (Just newN)
        incVer pred
        incVer cur
    insertAtEnd pred = do
        newN <- newNode key Nothing
        writeIORef (nextRef pred) (Just newN)
        incVer pred

-- | Delete a key if present.
delete :: Ord a => Node a -> a -> IO ()
delete headNode key = go
  where
    go = do
        (pred, mcur) <- findPos headNode key
        case mcur of
            Nothing -> return ()   -- not found
            Just cur -> do
                let locks = [pred, cur]
                withLocks locks $ do
                    predNext <- readIORef (nextRef pred)
                    curNext  <- readIORef (nextRef cur)
                    if predNext /= Just cur
                        then go   -- structure changed, retry
                        else if val cur /= key
                            then return ()   -- key already removed by another thread
                            else do
                                writeIORef (nextRef pred) curNext
                                incVer pred
                                incVer cur
