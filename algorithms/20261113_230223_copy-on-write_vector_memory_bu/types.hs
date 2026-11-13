module Types where

import Data.Array (Array)

-- | Internal buffer that may be shared among multiple vectors.
--   The 'shared' flag indicates whether the buffer is currently shared.
data Buffer a = Buffer
  { bufArray  :: Maybe (Array Int a)  -- ^ Underlying immutable array (Nothing for empty vector)
  , bufShared :: Bool                 -- ^ True if the buffer is shared
  }

-- | Copy‑On‑Write immutable vector.
data COWVector a = COWVector
  { vecSize   :: Int      -- ^ Number of elements stored
  , vecBuffer :: Buffer a -- ^ Underlying buffer
  }

-- | Simple assertion helper used in tests.
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)
