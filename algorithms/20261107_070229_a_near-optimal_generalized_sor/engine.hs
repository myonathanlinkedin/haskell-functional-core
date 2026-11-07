module Engine (
    sort,
    sortList
) where

import Types (Sortable(..))

-- Public entry point (re-exported from Types)
sort :: Ord a => [a] -> [a]
sort = sortList

-- Threshold for switching to insertion sort
threshold :: Int
threshold = 16

-- | Main introsort driver: quicksort with depth limit, falling back to merge sort,
--   and insertion sort for tiny partitions.
sortList :: Ord a => [a] -> [a]
sortList xs = introsort xs (maxDepth (length xs))

-- Compute a safe recursion depth: 2 * floor(log2 n)
maxDepth :: Int -> Int
maxDepth n = 2 * floorLog2 (max 1 n)

-- Integer floor of log base 2
floorLog2 :: Int -> Int
floorLog2 x
    | x <= 1    = 0
    | otherwise = 1 + floorLog2 (x `div` 2)

-- Introsort dispatcher
introsort :: Ord a => [a] -> Int -> [a]
introsort xs depth
    | length xs <= threshold = insertionSort xs
    | depth == 0             = mergeSort xs
    | otherwise              = case xs of
        []  -> []
        [_] -> xs
        _   -> let (pivot, rest) = medianOfThree xs
                   (lt, eq, gt)  = partition3 pivot rest
               in introsort lt (depth - 1) ++ eq ++ introsort gt (depth - 1)

-- | Median‑of‑three pivot selection (first, middle, last)
medianOfThree :: Ord a => [a] -> (a, [a])
medianOfThree xs = (pivot, withoutPivot)
  where
    first = head xs
    last' = last xs
    middle = xs !! (length xs `div` 2)
    trio = [(first,0), (middle,1), (last',2)]
    sortedTrio = map fst $ quicksortBy (compare `on` fst) trio
    pivot = sortedTrio !! 1
    -- Remove exactly one occurrence of the pivot from the original list
    withoutPivot = removeFirst pivot xs

    on f g x y = f (g x) (g y)

    quicksortBy _ [] = []
    quicksortBy cmp (p:ps) =
        let smaller = [x | x <- ps, cmp x p == LT]
            larger  = [x | x <- ps, cmp x p /= LT]
        in quicksortBy cmp smaller ++ [p] ++ quicksortBy cmp larger

    removeFirst _ [] = []
    removeFirst y (z:zs)
        | y == z    = zs
        | otherwise = z : removeFirst y zs

-- | Three‑way partition around a pivot.
partition3 :: Ord a => a -> [a] -> ([a], [a], [a])
partition3 pivot = foldr step ([], [], [])
  where
    step x (lt, eq, gt)
        | x < pivot = (x:lt, eq, gt)
        | x == pivot = (lt, x:eq, gt)
        | otherwise = (lt, eq, x:gt)

-- | Simple insertion sort for tiny lists.
insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []
  where
    insert x [] = [x]
    insert x ys@(y:ys')
        | x <= y    = x : ys
        | otherwise = y : insert x ys'

-- | Stable merge sort used as the fallback when recursion depth is exhausted.
mergeSort :: Ord a => [a] -> [a]
mergeSort xs
    | length xs <= 1 = xs
    | otherwise      = merge (mergeSort left) (mergeSort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs

    merge [] ys = ys
    merge xs [] = xs
    merge xs@(x:xt) ys@(y:yt)
        | x <= y    = x : merge xt ys
        | otherwise = y : merge xs yt
