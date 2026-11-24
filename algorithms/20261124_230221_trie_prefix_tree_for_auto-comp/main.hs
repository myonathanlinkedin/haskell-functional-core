module Main where

import Types (Trie, emptyTrie)
import Engine (insert, lookupFreq, autocomplete)

-- Simple assertion helper; aborts with an error message on failure.
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)

-- Equality check for lists of pairs, ignoring order when frequencies differ.
equalPairs :: Eq a => [(String, a)] -> [(String, a)] -> Bool
equalPairs xs ys = xs == ys

main :: IO ()
main = do
    -- Build a trie with some sample words and frequencies.
    let wordsWithFreq = [ ("cat", 5)
                        , ("car", 3)
                        , ("cart", 2)
                        , ("dog", 4)
                        , ("dot", 1)
                        , ("dove", 2)
                        , ("cater", 1)
                        ]
        trie0 = emptyTrie
        trie = foldr (\(w,f) t -> insert w f t) trie0 wordsWithFreq

    -- Test lookupFreq
    assert (lookupFreq "cat" trie == Just 5) "lookup cat"
    assert (lookupFreq "car" trie == Just 3) "lookup car"
    assert (lookupFreq "cater" trie == Just 1) "lookup cater"
    assert (lookupFreq "cow" trie == Nothing) "lookup missing word"

    -- Test autocomplete with various prefixes
    let ac1 = autocomplete "ca" 10 trie
    let expected1 = [ ("cat",5), ("car",3), ("cart",2), ("cater",1) ]
    assert (equalPairs ac1 expected1) "autocomplete ca"

    let ac2 = autocomplete "do" 10 trie
    let expected2 = [ ("dog",4), ("dove",2), ("dot",1) ]
    assert (equalPairs ac2 expected2) "autocomplete do"

    let ac3 = autocomplete "" 5 trie  -- top 5 overall
    let expected3 = [ ("cat",5), ("dog",4), ("car",3), ("cart",2), ("dove",2) ]
    assert (equalPairs ac3 expected3) "autocomplete empty prefix top 5"

    let ac4 = autocomplete "z" 10 trie
    assert (null ac4) "autocomplete non‑existent prefix"

    putStrLn "All tests passed."
