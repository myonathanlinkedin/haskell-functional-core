module Types (Trie, emptyTrie) where

import qualified Data.Map.Strict as Map

-- | A Trie node stores the frequency of a word ending at this node (0 if none)
--   and a map from characters to child sub-tries.
data Trie = Trie
    { freq     :: !Int
    , children :: !(Map.Map Char Trie)
    } deriving (Eq, Show)

-- | The empty trie has zero frequency and no children.
emptyTrie :: Trie
emptyTrie = Trie 0 Map.empty
