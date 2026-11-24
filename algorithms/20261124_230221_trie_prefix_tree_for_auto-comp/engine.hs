module Engine
    ( insert
    , lookupFreq
    , autocomplete
    ) where

import Types (Trie(..), emptyTrie)
import qualified Data.Map.Strict as Map
import Data.List (sortBy)
import Data.Ord (comparing)

-- | Insert a word into the trie, adding the given increment to its frequency.
--   If the word already exists, its frequency is increased by the increment.
insert :: String -> Int -> Trie -> Trie
insert [] inc (Trie f ch) = Trie (f + inc) ch
insert (c:cs) inc (Trie f ch) =
    let subTrie = Map.findWithDefault emptyTrie c ch
        newSub  = insert cs inc subTrie
    in Trie f (Map.insert c newSub ch)

-- | Look up the frequency of a word. Returns Nothing if the word is absent.
lookupFreq :: String -> Trie -> Maybe Int
lookupFreq [] (Trie f _) = if f > 0 then Just f else Nothing
lookupFreq (c:cs) (Trie _ ch) =
    case Map.lookup c ch of
        Nothing   -> Nothing
        Just sub  -> lookupFreq cs sub

-- | Return up to @limit@ completions for the given prefix, sorted by descending frequency.
--   Each result is a pair (completion, frequency) where the completion includes the prefix.
autocomplete :: String -> Int -> Trie -> [(String, Int)]
autocomplete prefix limit trie =
    case descend prefix trie of
        Nothing   -> []
        Just sub  -> take limit $ sortBy (flip (comparing snd)) (collect prefix sub)

-- Helper: descend the trie following the prefix; returns the sub-trie at that point.
descend :: String -> Trie -> Maybe Trie
descend [] t = Just t
descend (c:cs) (Trie _ ch) = Map.lookup c ch >>= descend cs

-- Helper: collect all words reachable from the given trie node,
--   prefixing each with the accumulated string.
collect :: String -> Trie -> [(String, Int)]
collect pref (Trie f ch) =
    let here = [(pref, f) | f > 0]
        deeper = concatMap (\(c, sub) -> collect (pref ++ [c]) sub) (Map.toList ch)
    in here ++ deeper
