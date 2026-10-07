module Main where

import Core
import Control.Monad (forM_)
import Data.List (sort)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO Bool
assertEqual name expected actual =
    if expected == actual
       then putStrLn (name ++ ": PASS") >> return True
       else putStrLn (name ++ ": FAIL (expected " ++ show expected ++ ", got " ++ show actual ++ ")") >> return False

runTests :: IO ()
runTests = do
    let t0 = empty :: Trie Int
        t1 = insert "cat" 1 $ insert "car" 2 $ insert "dog" 3 t0

    r1 <- assertEqual "lookup cat" (Just 1) (lookup "cat" t1)
    r2 <- assertEqual "lookup car" (Just 2) (lookup "car" t1)
    r3 <- assertEqual "lookup dog" (Just 3) (lookup "dog" t1)
    r4 <- assertEqual "lookup cow (missing)" Nothing (lookup "cow" t1)

    let t2 = delete "car" t1
    r5 <- assertEqual "lookup car after delete" Nothing (lookup "car" t2)
    r6 <- assertEqual "lookup cat still present" (Just 1) (lookup "cat" t2)

    let pref1 = sort $ keysWithPrefix "ca" t2
    r7 <- assertEqual "prefix ca after delete" ["cat"] pref1

    let t3 = insert "" 99 t2
    r8 <- assertEqual "lookup empty string" (Just 99) (lookup "" t3)

    let pref2 = sort $ keysWithPrefix "" t3
    r9 <- assertEqual "all keys" (sort ["", "cat", "dog"]) pref2

    let passed = length $ filter id [r1,r2,r3,r4,r5,r6,r7,r8,r9]
        total  = 9
    putStrLn $ "\nSummary: " ++ show passed ++ " / " ++ show total ++ " tests passed."

main :: IO ()
main = runTests
