module Main where

import Core
import System.Exit (exitFailure, exitSuccess)

-- Simple assertion helper.
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual msg expected actual =
    if expected == actual
        then putStrLn $ "[PASS] " ++ msg
        else do
            putStrLn $ "[FAIL] " ++ msg
            putStrLn $ "  Expected: " ++ show expected
            putStrLn $ "  Actual  : " ++ show actual
            exitFailure

-- Test insertion and lookup.
testInsertLookup :: IO ()
testInsertLookup = do
    let m0 = empty :: AetherMap String
        m1 = insert (2,3) "alpha" m0
        m2 = insert (1,5) "beta"  m1
        m3 = insert (2,3) "gamma" m2   -- overwrite
    assertEqual "lookup existing key (2,3)" (Just "gamma") (amLookup (2,3) m3)
    assertEqual "lookup existing key (1,5)" (Just "beta")  (amLookup (1,5) m3)
    assertEqual "lookup missing key" Nothing (amLookup (0,0) m3)

-- Test deletion.
testDelete :: IO ()
testDelete = do
    let kvs = [ ((3,1), "a")
              , ((2,2), "b")
              , ((5,0), "c")
              , ((1,4), "d")
              , ((4,3), "e")
              ]
        m = fromList kvs
        mDel = delete (2,2) $ delete (5,0) m
    assertEqual "deleted key absent" Nothing (amLookup (2,2) mDel)
    assertEqual "deleted key absent 2" Nothing (amLookup (5,0) mDel)
    assertEqual "remaining key present" (Just "a") (amLookup (3,1) mDel)

-- Test fromList / toList round‑trip and ordering.
testFromToList :: IO ()
testFromToList = do
    let kvs = [ ((10,0), "x")
              , ((5,5), "y")
              , ((5,2), "z")
              , ((0,0), "w")
              ]
        m = fromList kvs
        lst = toList m
        expected = [ ((0,0), "w")
                   , ((5,2), "z")
                   , ((5,5), "y")
                   , ((10,0), "x")
                   ]
    assertEqual "toList yields sorted ascending list" expected lst

-- Test that delete on a non‑existent key leaves the map unchanged.
testDeleteNonexistent :: IO ()
testDeleteNonexistent = do
    let m = fromList [ ((1,1), "a"), ((2,2), "b") ]
        m' = delete (3,3) m
    assertEqual "delete non‑existent key unchanged" (toList m) (toList m')

-- Entry point runs all tests.
main :: IO ()
main = do
    testInsertLookup
    testDelete
    testFromToList
    testDeleteNonexistent
    putStrLn "All tests passed."
    exitSuccess
