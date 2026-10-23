module Main where

import Core (CuckooFilter, emptyFilter, insert, delete, member)
import Data.Word (Word64)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (foldM, unless)

-- Simple assertion helper
assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False msg = putStrLn ("Assertion failed: " ++ msg) >> exitFailure

-- Test suite
main :: IO ()
main = do
    let bucketCount = 1024  -- must be power of two
        cf0 = emptyFilter bucketCount

        -- Sample keys
        keys :: [Word64]
        keys = [1..5000]

    -- Insert all keys
    cf1 <- foldM (\cf k -> case insert k cf of
                               Just cf' -> return cf'
                               Nothing  -> putStrLn ("Insert failed for key " ++ show k) >> exitFailure)
                 cf0 keys

    -- Verify presence
    mapM_ (\k -> assert (member k cf1) ("Key not found after insert: " ++ show k)) keys

    -- Delete a subset
    let toDelete = take 1000 keys
        cf2 = foldl (flip delete) cf1 toDelete

    -- Verify deletions
    mapM_ (\k -> assert (not (member k cf2)) ("Key still present after delete: " ++ show k)) toDelete

    -- Verify remaining keys still present
    let remaining = drop 1000 keys
    mapM_ (\k -> assert (member k cf2) ("Remaining key missing: " ++ show k)) remaining

    putStrLn "All tests passed."
    exitSuccess
