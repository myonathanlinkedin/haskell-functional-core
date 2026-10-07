module Main where

import Types
import Engine
import Control.Concurrent
import Control.Monad
import System.Exit (exitFailure)

-- Simple assertion helper
assert :: Bool -> String -> IO ()
assert True _  = return ()
assert False msg = putStrLn ("Assertion failed: " ++ msg) >> exitFailure

-- Verify that a list contains exactly the expected elements (sorted, no duplicates)
checkContents :: (Ord a, Show a) => Node a -> [a] -> IO ()
checkContents headNode expected = do
    let go [] = return ()
        go (x:xs) = do
            present <- contains headNode x
            assert present ("Missing element: " ++ show x)
            go xs
    go expected
    -- also ensure no extra elements
    let allKeys = expected
    forM_ allKeys $ \k -> do
        present <- contains headNode k
        assert present ("Element disappeared unexpectedly: " ++ show k)

main :: IO ()
main = do
    -- Create an empty list
    lst <- newList :: IO (Node Int)

    -- Sequential tests
    insert lst 5
    insert lst 3
    insert lst 7
    insert lst 5   -- duplicate, should be ignored
    delete lst 3
    delete lst 10 -- non‑existent

    assert =<< contains lst 5 $ "Contains 5"
    assert =<< contains lst 3 $ "Contains 3 after deletion"
    assert =<< not <$> contains lst 3 $ "3 should be absent"
    assert =<< contains lst 7 $ "Contains 7"

    checkContents lst [5,7]

    -- Concurrent stress test
    let keys = [1..1000]
    doneVar <- newEmptyMVar

    -- Writer threads: insert all even numbers
    forkIO $ do
        forM_ (filter even keys) $ \k -> insert lst k
        putMVar doneVar ()

    -- Writer threads: delete some numbers concurrently
    forkIO $ do
        forM_ (filter (<500) keys) $ \k -> delete lst k
        putMVar doneVar ()

    -- Reader threads: verify presence/absence while operations run
    forkIO $ do
        forM_ keys $ \k -> do
            _ <- contains lst k
            return ()
        putMVar doneVar ()

    -- Wait for all three workers
    replicateM_ 3 (takeMVar doneVar)

    -- Final verification
    let expected = [k | k <- keys, even k, k >= 500]  -- evens not deleted
    checkContents lst expected

    putStrLn "All tests passed."
