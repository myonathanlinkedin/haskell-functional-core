module Main where

import Engine (testTrie, buildTrie, suggest)
import Types (Trie)

-- Simple assertion helper that aborts on failure.
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)

main :: IO ()
main = do
    -- Run internal test suite
    if testTrie
        then putStrLn "All internal tests passed."
        else error "Internal tests failed."

    -- Demonstration of usage
    let dict = [ ("apple", 10)
               , ("app", 15)
               , ("application", 5)
               , ("ape", 7)
               , ("banana", 8)
               , ("band", 3)
               , ("bandana", 4)
               , ("bandit", 2)
               , ("cat", 12)
               , ("caterpillar", 1)
               ]
        trie = buildTrie dict

    putStrLn "\nAutocomplete examples:"
    let demo prefix limit = do
            let results = suggest limit prefix trie
            putStrLn $ "Prefix \"" ++ prefix ++ "\" (limit " ++ show limit ++ "): " ++ show results

    demo "app" 5   -- expect ["app","apple","application","ape"]
    demo "ban" 3   -- expect ["banana","bandana","band"]
    demo "" 4      -- top 4 overall frequencies

    -- Additional sanity checks using assert
    assert (suggest 3 "app" trie == ["app","apple","application"]) "Demo check 1"
    assert (suggest 2 "cat" trie == ["cat","caterpillar"]) "Demo check 2"

    putStrLn "\nDemo completed successfully."
