module Core (
    CMS,
    Hashable(..),
    emptyCMS,
    add,
    estimate,
    heavyHitters
) where

import Data.Array (Array, array, (!), (//))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Bits (xor)
import Data.List (foldl')

-- | Simple hash class – we provide instances for common types.
class Hashable a where
    hash :: a -> Int

instance Hashable Int where
    hash = id

instance Hashable Integer where
    hash = fromIntegral . (`mod` toInteger (maxBound :: Int))

instance Hashable Char where
    hash = fromEnum

instance Hashable String where
    hash = foldl' (\h c -> h * 31 + fromEnum c) 0

-- | Count‑Min Sketch with an auxiliary candidate map for heavy‑hitter queries.
data CMS a = CMS
    { depth      :: !Int               -- ^ Number of hash rows (d)
    , width      :: !Int               -- ^ Number of counters per row (w)
    , seeds      :: ![Int]             -- ^ Pairwise‑independent seeds, length = d
    , table      :: !(Array (Int,Int) Int) -- ^ Counter matrix
    , candidates :: !(Map a Int)      -- ^ Approximate counts for observed items
    }

-- Large prime for modular hashing (fits in 32‑bit signed Int)
prime :: Int
prime = 2147483647

-- | Compute the column index for a given row seed.
hashIdx :: Hashable a => Int -> Int -> a -> Int
hashIdx w seed x = ((hash x `xor` seed) `mod` prime) `mod` w

-- | Create an empty sketch. Width and depth must be positive.
emptyCMS :: Hashable a => Int -> Int -> CMS a
emptyCMS w d
    | w <= 0 || d <= 0 = error "emptyCMS: width and depth must be > 0"
    | otherwise        = CMS
        { depth = d
        , width = w
        , seeds = take d $ iterate (\s -> (s * 2654435761) `mod` prime) 1
        , table = array ((0,0),(d-1,w-1))
                    [ ((i,j), 0) | i <- [0..d-1], j <- [0..w-1] ]
        , candidates = Map.empty
        }

-- | Increment the count of an element by a positive amount.
add :: Hashable a => a -> Int -> CMS a -> CMS a
add _  inc _ | inc <= 0 = error "add: increment must be > 0"
add x inc cms = cms { table = newTable, candidates = newCand }
  where
    d   = depth cms
    w   = width cms
    s   = seeds cms
    tbl = table cms
    -- Update each row counter
    updates = [ ((i, idx i), tbl ! (i, idx i) + inc)
                | i <- [0..d-1]
                , let idx i = hashIdx w (s !! i) x
              ]
    newTable = tbl // updates
    -- Update candidate map with the *new* estimate (upper bound)
    estNow = estimate x cms
    newCand = Map.insertWith max x (estNow + inc) (candidates cms)

-- | Query the estimated frequency of an element (upper bound).
estimate :: Hashable a => a -> CMS a -> Int
estimate x cms = minimum
    [ table cms ! (i, idx i)
      | i <- [0..depth cms - 1]
      , let idx i = hashIdx (width cms) (seeds cms !! i) x
    ]

-- | Return all items whose estimated count is at least the given threshold.
heavyHitters :: Int -> CMS a -> [(a, Int)]
heavyHitters thresh cms
    | thresh <= 0 = error "heavyHitters: threshold must be > 0"
    | otherwise   = [ (k,v) | (k,v) <- Map.toList (candidates cms), v >= thresh ]
