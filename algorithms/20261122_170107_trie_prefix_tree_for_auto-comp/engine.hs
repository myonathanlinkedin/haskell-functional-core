module Engine (
    buildTrie,
    suggest,
    testTrie
) where

import Types (Trie, emptyTrie, insert, lookupWord, autocomplete)

-- | Build a trie from a list of (word, frequency) pairs.
buildTrie :: [(String, Int)] -> Trie
buildTrie = foldl (\t (w,f) -> insert w f t) emptyTrie

-- | Suggest completions for a prefix, limiting the number of results.
suggest :: Int -> String -> Trie -> [String]
suggest limit pref = autocomplete pref limit

-- | A simple test suite returning True if all assertions hold.
testTrie :: Bool
testTrie =
    let wordsFreq = [ ("cat", 5)
                    , ("car", 3)
                    , ("cart", 2)
                    , ("dog", 4)
                    , ("dot", 1)
                    , ("dove", 6)
                    , ("cater", 1)
                    ]
        trie = buildTrie wordsFreq

        -- Helper for assertions
        assertEq :: (Eq a, Show a) => String -> a -> a -> Bool
        assertEq _ expected actual = expected == actual

        -- 1. Lookup tests
        testLookup = and [
            assertEq "lookup cat" (Just 5) (lookupWord "cat" trie),
            assertEq "lookup car" (Just 3) (lookupWord "car" trie),
            assertEq "lookup cart" (Just 2) (lookupWord "cart" trie),
            assertEq "lookup dog" (Just 4) (lookupWord "dog" trie),
            assertEq "lookup unknown" Nothing (lookupWord "cow" trie)
            ]

        -- 2. Autocomplete tests
        testAuto1 = autocomplete "ca" 10 trie == ["cat","car","cater","cart"]
        testAuto2 = autocomplete "do" 2 trie == ["dove","dog"]
        testAuto3 = autocomplete "" 3 trie == ["dove","cat","dog"]  -- top 3 overall
        testAuto4 = autocomplete "z" 5 trie == []                -- no matches

    in and [testLookup, testAuto1, testAuto2, testAuto3, testAuto4]
