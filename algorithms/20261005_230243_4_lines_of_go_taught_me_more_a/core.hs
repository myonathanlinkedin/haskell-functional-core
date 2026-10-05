module Core (RingBuffer, newRingBuffer, push, pop) where

import Data.Array.IO (IOArray, newArray, readArray, writeArray)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Concurrent (MVar, newMVar, withMVar)

-- | A simple thread‑safe ring buffer using a mutex.
data RingBuffer a = RingBuffer
  { capacity :: Int
  , buffer   :: IOArray Int (IORef (Maybe a))
  , headIdx  :: IORef Int
  , tailIdx  :: IORef Int
  , lock     :: MVar ()
  }

-- | Create a new ring buffer with the given capacity (must be > 0).
newRingBuffer :: Int -> IO (RingBuffer a)
newRingBuffer cap
  | cap <= 0 = error "capacity must be positive"
  | otherwise = do
      arr <- newArray (0, cap - 1) =<< newIORef Nothing
      h   <- newIORef 0
      t   <- newIORef 0
      m   <- newMVar ()
      return $ RingBuffer cap arr h t m

-- | Push an element onto the buffer.
-- Returns 'True' on success, 'False' if the buffer is full.
push :: RingBuffer a -> a -> IO Bool
push rb x = withMVar (lock rb) $ \_ -> do
  t <- readIORef (tailIdx rb)
  h <- readIORef (headIdx rb)
  let next = (t + 1) `mod` capacity rb
  if next == h
    then return False               -- full
    else do
      cell <- readArray (buffer rb) t
      writeIORef cell (Just x)
      writeIORef (tailIdx rb) next
      return True

-- | Pop an element from the buffer.
-- Returns 'Just a' if an element was available, otherwise 'Nothing'.
pop :: RingBuffer a -> IO (Maybe a)
pop rb = withMVar (lock rb) $ \_ -> do
  h <- readIORef (headIdx rb)
  t <- readIORef (tailIdx rb)
  if h == t
    then return Nothing            -- empty
    else do
      cell <- readArray (buffer rb) h
      mval <- readIORef cell
      writeIORef cell Nothing
      writeIORef (headIdx rb) ((h + 1) `mod` capacity rb)
      return mval
