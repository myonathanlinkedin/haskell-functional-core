module Main where

import Engine (sort)
import System.Random (mkStdGen, randomRs)
import Control.Exception (assert)

-- Simple assertion helper
assert' :: Bool -> String -> IO ()
assert' True  _   = return ()
assert' False msg = error ("Assertion failed: " ++ msg)

-- Verify that a list is sorted in non‑decreasing order
isSorted :: Ord a => [a] -> Bool
isSorted []       = True
isSorted [_]      = True
isSorted (x:y:zs) = x <= y && isSorted (y:zs)

-- Verify that two lists are permutations of each other (by sorting both)
isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation xs ys = sort xs == sort ys

-- Deterministic random list generator
randomList :: Int -> [Int]
randomList n = take n $ randomRs (0, 1000000) (mkStdGen 42)

main :: IO ()
main = do
    -- Edge cases
    let empty      = ([] :: [Int])
        singleton  = [42]
        sortedAsc  = [1..100]
        sortedDesc = reverse sortedAsc
        duplicates = [5,1,3,5,2,1,4,5,3,2]

    -- Large random list
    let large = randomList 10000

    -- Test suite
    let tests = [ (empty, "empty list")
                , (singleton, "singleton")
                , (sortedAsc, "already sorted")
                , (sortedDesc, "reverse sorted")
                , (duplicates, "duplicates")
                , (large, "large random")
                ]

    mapM_ (\(xs, name) -> do
        let ys = sort xs
        assert' (isSorted ys) ("list not sorted: " ++ name)
        assert' (isPermutation xs ys) ("permutation broken: " ++ name)
        putStrLn ("[PASS] " ++ name)
        ) tests

    putStrLn "All tests passed."

// End of files.
