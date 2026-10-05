module Main where

import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad
import System.Exit (exitFailure)
import Data.Maybe (isNothing, fromJust)

-- Node of doubly linked list
data Node k v = Node
  { nPrev :: IORef (Maybe (Node k v))
  , nNext :: IORef (Maybe (Node k v))
  , nKey  :: k
  , nVal  :: IORef v
  }

-- LRU Cache structure
data LRUCache k v = LRUCache
  { capacity :: Int
  , sizeRef  :: IORef Int
  , mapRef   :: IORef (Map k (Node k v))
  , headRef  :: IORef (Maybe (Node k v))  -- most recent
  , tailRef  :: IORef (Maybe (Node k v))  -- least recent
  }

newCache :: Int -> IO (LRUCache k v)
newCache cap = do
  sz <- newIORef 0
  mp <- newIORef Map.empty
  hd <- newIORef Nothing
  tl <- newIORef Nothing
  return $ LRUCache cap sz mp hd tl

-- Internal helpers ---------------------------------------------------------

removeNode :: LRUCache k v -> Node k v -> IO ()
removeNode cache node = do
  mPrev <- readIORef (nPrev node)
  mNext <- readIORef (nNext node)

  case mPrev of
    Just p -> writeIORef (nNext p) mNext
    Nothing -> writeIORef (headRef cache) mNext

  case mNext of
    Just n -> writeIORef (nPrev n) mPrev
    Nothing -> writeIORef (tailRef cache) mPrev

  modifyIORef' (sizeRef cache) (subtract 1)

addToFront :: LRUCache k v -> Node k v -> IO ()
addToFront cache node = do
  writeIORef (nPrev node) Nothing
  mHead <- readIORef (headRef cache)
  writeIORef (nNext node) mHead
  case mHead of
    Just h -> writeIORef (nPrev h) (Just node)
    Nothing -> writeIORef (tailRef cache) (Just node)
  writeIORef (headRef cache) (Just node)
  modifyIORef' (sizeRef cache) (+1)

moveToFront :: LRUCache k v -> Node k v -> IO ()
moveToFront cache node = do
  mHead <- readIORef (headRef cache)
  when (Just node /= mHead) $ do
    removeNode cache node
    addToFront cache node

evictIfNeeded :: (Ord k) => LRUCache k v -> IO ()
evictIfNeeded cache = do
  sz <- readIORef (sizeRef cache)
  when (sz > capacity cache) $ do
    mTail <- readIORef (tailRef cache)
    case mTail of
      Just t -> do
        removeNode cache t
        modifyIORef' (mapRef cache) (Map.delete (nKey t))
      Nothing -> return ()

-- Public API --------------------------------------------------------------

get :: (Ord k) => LRUCache k v -> k -> IO (Maybe v)
get cache k = do
  mp <- readIORef (mapRef cache)
  case Map.lookup k mp of
    Just node -> do
      moveToFront cache node
      v <- readIORef (nVal node)
      return (Just v)
    Nothing -> return Nothing

put :: (Ord k) => LRUCache k v -> k -> v -> IO ()
put cache k v = do
  mp <- readIORef (mapRef cache)
  case Map.lookup k mp of
    Just node -> do
      writeIORef (nVal node) v
      moveToFront cache node
    Nothing -> do
      p <- newIORef Nothing
      n <- newIORef Nothing
      valRef <- newIORef v
      let node = Node p n k valRef
      addToFront cache node
      modifyIORef' (mapRef cache) (Map.insert k node)
      evictIfNeeded cache

delete :: (Ord k) => LRUCache k v -> k -> IO ()
delete cache k = do
  mp <- readIORef (mapRef cache)
  case Map.lookup k mp of
    Just node -> do
      removeNode cache node
      modifyIORef' (mapRef cache) (Map.delete k)
    Nothing -> return ()

toList :: LRUCache k v -> IO [(k, v)]
toList cache = do
  mHead <- readIORef (headRef cache)
  let go Nothing acc = return (reverse acc)
      go (Just node) acc = do
        v <- readIORef (nVal node)
        mNext <- readIORef (nNext node)
        go mNext ((nKey node, v) : acc)
  go mHead []

-- Simple test framework ----------------------------------------------------

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual msg expected actual =
  unless (expected == actual) $ do
    putStrLn $ "Assertion failed: " ++ msg
    putStrLn $ "  Expected: " ++ show expected
    putStrLn $ "  Actual:   " ++ show actual
    exitFailure

-- Unit tests --------------------------------------------------------------

runTests :: IO ()
runTests = do
  cache <- newCache 2

  put cache 1 "one"
  put cache 2 "two"
  lst1 <- toList cache
  assertEqual "After two inserts" [(2,"two"),(1,"one")] lst1

  m1 <- get cache 1
  assertEqual "Get existing key 1" (Just "one") m1
  lst2 <- toList cache
  assertEqual "Order after get 1" [(1,"one"),(2,"two")] lst2

  put cache 3 "three"  -- should evict key 2
  lst3 <- toList cache
  assertEqual "After inserting 3, evict 2" [(3,"three"),(1,"one")] lst3

  m2 <- get cache 2
  assertEqual "Get evicted key 2" Nothing m2

  delete cache 1
  lst4 <- toList cache
  assertEqual "After deleting 1" [(3,"three")] lst4

  put cache 4 "four"
  put cache 5 "five"  -- capacity 2, should keep 5 and 4
  lst5 <- toList cache
  assertEqual "After inserting 4 and 5" [(5,"five"),(4,"four")] lst5

  put cache 5 "FIVE"  -- update existing
  lst6 <- toList cache
  assertEqual "After updating 5" [(5,"FIVE"),(4,"four")] lst6

  put cache 6 "six"   -- evict 4
  lst7 <- toList cache
  assertEqual "After inserting 6, evict 4" [(6,"six"),(5,"FIVE")] lst7

  put cache 7 "seven"
  put cache 8 "eight"
  put cache 9 "nine"
  lst8 <- toList cache
  assertEqual "Multiple evictions keep last two" [(9,"nine"),(8,"eight")] lst8

  put cache 9 "NINE"
  lst9 <- toList cache
  assertEqual "Update most recent does not change order" [(9,"NINE"),(8,"eight")] lst9

  put cache 10 "ten"
  lst10 <- toList cache
  assertEqual "Insert new, evict least recent (8)" [(10,"ten"),(9,"NINE")] lst10

  put cache 11 "eleven"
  put cache 12 "twelve"
  lst11 <- toList cache
  assertEqual "Final state with capacity 2" [(12,"twelve"),(11,"eleven")] lst11

  put cache 13 "thirteen"
  lst12 <- toList cache
  assertEqual "After inserting 13, evict 11" [(13,"thirteen"),(12,"twelve")] lst12

  put cache 14 "fourteen"
  put cache 15 "fifteen"
  lst13 <- toList cache
  assertEqual "After inserting 14 and 15, keep 15 and 14" [(15,"fifteen"),(14,"fourteen")] lst13

  put cache 16 "sixteen"
  lst14 <- toList cache
  assertEqual "Insert 16 evicts 14" [(16,"sixteen"),(15,"fifteen")] lst14

  put cache 17 "seventeen"
  put cache 18 "eighteen"
  lst15 <- toList cache
  assertEqual "Insert 17,18 evicts 15,16" [(18,"eighteen"),(17,"seventeen")] lst15

  put cache 19 "nineteen"
  lst16 <- toList cache
  assertEqual "Insert 19 evicts 17" [(19,"nineteen"),(18,"eighteen")] lst16

  put cache 20 "twenty"
  lst17 <- toList cache
  assertEqual "Insert 20 evicts 18" [(20,"twenty"),(19,"nineteen")] lst17

  put cache 21 "twenty-one"
  lst18 <- toList cache
  assertEqual "Insert 21 evicts 19" [(21,"twenty-one"),(20,"twenty")] lst18

  put cache 22 "twenty-two"
  lst19 <- toList cache
  assertEqual "Insert 22 evicts 20" [(22,"twenty-two"),(21,"twenty-one")] lst19

  put cache 23 "twenty-three"
  lst20 <- toList cache
  assertEqual "Insert 23 evicts 21" [(23,"twenty-three"),(22,"twenty-two")] lst20

  put cache 24 "twenty-four"
  lst21 <- toList cache
  assertEqual "Insert 24 evicts 22" [(24,"twenty-four"),(23,"twenty-three")] lst21

  put cache 25 "twenty-five"
  lst22 <- toList cache
  assertEqual "Insert 25 evicts 23" [(25,"twenty-five"),(24,"twenty-four")] lst22

  put cache 26 "twenty-six"
  lst23 <- toList cache
  assertEqual "Insert 26 evicts 24" [(26,"twenty-six"),(25,"twenty-five")] lst23

  put cache 27 "twenty-seven"
  lst24 <- toList cache
  assertEqual "Insert 27 evicts 25" [(27,"twenty-seven"),(26,"twenty-six")] lst24

  put cache 28 "twenty-eight"
  lst25 <- toList cache
  assertEqual "Insert 28 evicts 26" [(28,"twenty-eight"),(27,"twenty-seven")] lst25

  put cache 29 "twenty-nine"
  lst26 <- toList cache
  assertEqual "Insert 29 evicts 27" [(29,"twenty-nine"),(28,"twenty-eight")] lst26

  put cache 30 "thirty"
  lst27 <- toList cache
  assertEqual "Insert 30 evicts 28" [(30,"thirty"),(29,"twenty-nine")] lst27

  put cache 31 "thirty-one"
  lst28 <- toList cache
  assertEqual "Insert 31 evicts 29" [(31,"thirty-one"),(30,"thirty")] lst28

  put cache 32 "thirty-two"
  lst29 <- toList cache
  assertEqual "Insert 32 evicts 30" [(32,"thirty-two"),(31,"thirty-one")] lst29

  put cache 33 "thirty-three"
  lst30 <- toList cache
  assertEqual "Insert 33 evicts
