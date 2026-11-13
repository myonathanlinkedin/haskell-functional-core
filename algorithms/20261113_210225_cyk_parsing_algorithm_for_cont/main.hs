module Main where

import Prelude
import CYK
import System.Exit (exitFailure, exitSuccess)

-- Simple assertion helper.
assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False msg = putStrLn ("Assertion failed: " ++ msg) >> exitFailure

-- Grammar 1: generates exactly the string "ab".
grammar1 :: Grammar
grammar1 =
    [ TermProd "A" 'a'
    , TermProd "B" 'b'
    , BinProd  "S" "A" "B"
    ]

-- Grammar 2: generates a^n b^n for any n ≥ 1.
--   S → A B | S S
--   A → a
--   B → b
grammar2 :: Grammar
grammar2 =
    [ TermProd "A" 'a'
    , TermProd "B" 'b'
    , BinProd  "S" "A" "B"
    , BinProd  "S" "S" "S"
    ]

runTests :: IO ()
runTests = do
    -- Tests for grammar1
    assert (cyk grammar1 "S" "ab") "grammar1 should accept \"ab\""
    assert (not (cyk grammar1 "S" "a")) "grammar1 should reject \"a\""
    assert (not (cyk grammar1 "S" "b")) "grammar1 should reject \"b\""
    assert (not (cyk grammar1 "S" "aba")) "grammar1 should reject \"aba\""

    -- Tests for grammar2 (a^n b^n)
    let accept = ["ab", "aabb", "aaabbb", "aaaabbbb"]
    let reject = ["a", "b", "abb", "aab", "aaabbbbbb", "ba"]
    mapM_ (\s -> assert (cyk grammar2 "S" s) ("grammar2 should accept " ++ show s)) accept
    mapM_ (\s -> assert (not (cyk grammar2 "S" s)) ("grammar2 should reject " ++ show s)) reject

    putStrLn "All tests passed."

main :: IO ()
main = runTests >> exitSuccess
