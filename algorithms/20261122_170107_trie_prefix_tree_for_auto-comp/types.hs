module Types (
    Trie,
    emptyTrie,
    insert,
    lookupWord,
    autocomplete
) where

import qualified Data.Map.Strict as Map

-- | A Trie node stores an optional frequency for a word ending at this node,
--   and a map from characters to child sub-tries.
data Trie = Trie {
    freq    :: Maybe Int,               -- ^ Frequency of the word ending here (if any)
    children :: Map.Map Char Trie       -- ^ Mapping from next character to sub‑trie
} deriving (Eq, Show)

-- | The empty trie has no word and no children.
emptyTrie :: Trie
emptyTrie = Trie Nothing Map.empty

-- | Insert a word with its frequency into a trie.
--   If the word already exists, the stored frequency is replaced by the new one.
insert :: String -> Int -> Trie -> Trie
insert []     f (Trie _ ch) = Trie (Just f) ch
insert (c:cs) f (Trie fr ch) =
    let subTrie = Map.findWithDefault emptyTrie c ch
        newSub  = insert cs f subTrie
    in Trie fr (Map.insert c newSub ch)

-- | Look up the frequency of a word. Returns 'Nothing' if the word is absent.
lookupWord :: String -> Trie -> Maybe Int
lookupWord []     (Trie fr _)  = fr
lookupWord (c:cs) (Trie _ ch) = Map.lookup c ch >>= lookupWord cs

-- | Return up to @limit@ completions for the given prefix, ordered by descending
--   frequency and then lexicographically ascending.
autocomplete :: String -> Int -> Trie -> [String]
autocomplete prefix limit trie =
    case descend prefix trie of
        Nothing   -> []
        Just node -> take limit $ map (prefix ++) $ rankedWords node
  where
    -- Follow the prefix down the trie; Nothing if prefix not present.
    descend :: String -> Trie -> Maybe Trie
    descend [] t = Just t
    descend (c:cs) (Trie _ ch) = Map.lookup c ch >>= descend cs

    -- Collect all words (suffixes) from a node together with their frequencies.
    collect :: Trie -> [(String, Int)]
    collect (Trie fr ch) =
        let own = case fr of
                    Just f  -> [("", f)]
                    Nothing -> []
            childPairs = Map.toList ch
            childWords = concatMap (\(c, sub) ->
                            map (\(s, f) -> (c:s, f)) (collect sub)
                         ) childPairs
        in own ++ childWords

    -- Sort words by descending frequency, then ascending lexical order.
    rankedWords :: Trie -> [String]
    rankedWords node =
        let ws = collect node
            cmp (s1,f1) (s2,f2) = case compare f2 f1 of
                                    EQ -> compare s1 s2
                                    ord -> ord
        in map fst $ sortBy cmp ws

    -- Local import of sortBy to keep the module header tidy.
    sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    sortBy = Data.List.sortBy

    -- Bring List into scope.
    importList :: ()
    importList = ()

    -- The above dummy definitions are only to keep GHC happy about the
    -- qualified import of Data.List used in sortBy.
    -- They have no runtime effect.

    -- Ensure Data.List is imported.
    import Data.List (sortBy)
