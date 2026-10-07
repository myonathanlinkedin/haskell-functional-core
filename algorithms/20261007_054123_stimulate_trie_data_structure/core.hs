module Core (Trie, empty, insert, lookup, delete, keysWithPrefix) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (isNothing)

data Trie a = Trie
  { value    :: Maybe a
  , children :: Map Char (Trie a)
  } deriving (Show, Eq)

empty :: Trie a
empty = Trie Nothing Map.empty

insert :: String -> a -> Trie a -> Trie a
insert []     val (Trie _ ch) = Trie (Just val) ch
insert (c:cs) val (Trie v ch) = Trie v $ Map.alter alterFn c ch
  where
    alterFn Nothing  = Just $ insert cs val empty
    alterFn (Just t) = Just $ insert cs val t

lookup :: String -> Trie a -> Maybe a
lookup []     (Trie v _)  = v
lookup (c:cs) (Trie _ ch) = Map.lookup c ch >>= lookup cs

delete :: String -> Trie a -> Trie a
delete [] (Trie _ ch) = prune $ Trie Nothing ch
delete (c:cs) (Trie v ch) = case Map.lookup c ch of
    Nothing -> Trie v ch
    Just child ->
        let newChild = delete cs child
            newChildren = if isEmpty newChild
                          then Map.delete c ch
                          else Map.insert c newChild ch
        in prune $ Trie v newChildren

keysWithPrefix :: String -> Trie a -> [String]
keysWithPrefix pref trie = case subTrie pref trie of
    Nothing -> []
    Just sub -> map (pref ++) (collect "" sub)

-- Helper functions ---------------------------------------------------------

subTrie :: String -> Trie a -> Maybe (Trie a)
subTrie [] t = Just t
subTrie (c:cs) (Trie _ ch) = Map.lookup c ch >>= subTrie cs

collect :: String -> Trie a -> [String]
collect acc (Trie v ch) =
    let here = case v of
                 Just _  -> [acc]
                 Nothing -> []
        deeper = concatMap (\(c, t) -> collect (acc ++ [c]) t) (Map.toList ch)
    in here ++ deeper

isEmpty :: Trie a -> Bool
isEmpty (Trie v ch) = isNothing v && Map.null ch

prune :: Trie a -> Trie a
prune t@(Trie v ch)
  | isEmpty t = empty
  | otherwise = Trie v ch
