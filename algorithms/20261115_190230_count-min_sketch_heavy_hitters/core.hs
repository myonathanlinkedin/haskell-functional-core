module Core (
    CountMinSketch,
    mkCMS,
    add,
    estimate,
    heavyHitters
) where

import Data.List (foldl')
import Data.Maybe (catMaybes)

-- | A simple Count-Min Sketch data structure.
data CountMinSketch a = CMS {
    depth     :: Int,               -- ^ Number of hash functions / rows
    width     :: Int,               -- ^ Number of counters per row
    hashFuncs :: [a -> Int],        -- ^ List of hash functions (one per row)
    table     :: [[Int]]            -- ^ 2D table of counters (rows of columns)
} deriving (Show, Eq)

-- | Large prime for universal hashing (greater than typical input range).
prime :: Int
prime = 2147483647  -- 2^31 - 1, a Mersenne prime

-- | Create a hash function from a pair of seeds (a,b).
mkHash :: Int -> Int -> Int -> (Int -> Int)
mkHash w a b = \x -> ((a * x + b) `mod` prime) `mod` w

-- | Construct an empty Count-Min Sketch.
--   The list of seed pairs must have length equal to the desired depth.
mkCMS :: (Integral a) => Int               -- ^ depth (d)
                     -> Int               -- ^ width (w)
                     -> [(Int,Int)]       -- ^ list of (a,b) seeds, length = d
                     -> CountMinSketch a
mkCMS d w seeds
    | length seeds /= d = error "Number of seeds must equal depth"
    | otherwise = CMS {
        depth = d,
        width = w,
        hashFuncs = map (\(a,b) -> mkHash w a b . fromIntegral) seeds,
        table = replicate d (replicate w 0)
    }

-- | Increment the counter at a specific index in a row.
incAt :: Int -> [Int] -> [Int]
incAt idx row =
    let (before, x:after) = splitAt idx row
    in before ++ (x+1) : after

-- | Add an element to the sketch, returning a new sketch.
add :: (Integral a) => CountMinSketch a -> a -> CountMinSketch a
add cms x = cms { table = newTable }
  where
    hs = hashFuncs cms
    oldTable = table cms
    newTable = zipWith updateRow hs oldTable
    updateRow h row = incAt (h x) row

-- | Estimate the frequency of an element (minimum over rows).
estimate :: (Integral a) => CountMinSketch a -> a -> Int
estimate cms x = minimum $ zipWith getCount hs tbl
  where
    hs = hashFuncs cms
    tbl = table cms
    getCount h row = row !! h x

-- | Retrieve heavy hitters from a list of candidate items.
--   An item is reported if its estimated frequency >= threshold.
heavyHitters :: (Integral a) => CountMinSketch a   -- ^ sketch
                                 -> Int           -- ^ frequency threshold
                                 -> [a]           -- ^ candidate items
                                 -> [(a,Int)]     -- ^ (item, estimated frequency)
heavyHitters cms thresh candidates =
    catMaybes $ map maybeHit candidates
  where
    maybeHit item =
        let est = estimate cms item
        in if est >= thresh then Just (item, est) else Nothing
