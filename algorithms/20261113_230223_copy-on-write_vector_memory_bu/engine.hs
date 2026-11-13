module Engine
  ( empty
  , fromList
  , lengthCOW
  , get
  , set
  , pushBack
  ) where

import Types
import Data.Array (Array, listArray, bounds, (!), (//), assocs, array)

-- | Create an empty vector.
empty :: COWVector a
empty = COWVector 0 (Buffer Nothing False)

-- | Build a vector from a list.
fromList :: [a] -> COWVector a
fromList xs = COWVector n (Buffer (Just arr) False)
  where
    n   = length xs
    arr = listArray (0, n - 1) xs

-- | Return the number of elements.
lengthCOW :: COWVector a -> Int
lengthCOW = vecSize

-- | Retrieve the element at the given index (0‑based).
--   Throws an error if the index is out of bounds.
get :: COWVector a -> Int -> a
get (COWVector n (Buffer (Just arr) _)) i
  | i < 0 || i >= n = error "get: index out of bounds"
  | otherwise       = arr ! i
get (COWVector _ (Buffer Nothing _)) _ = error "get: empty vector"

-- | Ensure the buffer is unique (i.e. not shared). If it is shared,
--   a fresh copy of the underlying array is created.
ensureUnique :: Buffer a -> Buffer a
ensureUnique buf@(Buffer (Just a) True) =
  Buffer (Just (array (bounds a) (assocs a))) False
ensureUnique buf = buf

-- | Update the element at the given index, returning a new vector.
--   The original vector remains unchanged (copy‑on‑write semantics).
set :: COWVector a -> Int -> a -> COWVector a
set v@(COWVector n buf) i val
  | i < 0 || i >= n = error "set: index out of bounds"
  | otherwise       =
      let buf'@(Buffer (Just arr') _) = ensureUnique buf
          newArr = arr' // [(i, val)]
      in COWVector n (buf' { bufArray = Just newArr })

-- | Append an element to the end of the vector, returning a new vector.
pushBack :: COWVector a -> a -> COWVector a
pushBack (COWVector n buf) val =
  let buf'@(Buffer maybeArr _) = ensureUnique buf
      newArr = case maybeArr of
        Nothing -> listArray (0,0) [val]
        Just arr ->
          let (l, _) = bounds arr
              newBounds = (l, n)
          in array newBounds (assocs arr ++ [(n, val)])
  in COWVector (n + 1) (buf' { bufArray = Just newArr })
