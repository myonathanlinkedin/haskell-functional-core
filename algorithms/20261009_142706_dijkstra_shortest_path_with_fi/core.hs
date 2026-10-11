module Core (
    Heap,
    empty,
    insert,
    findMin,
    deleteMin,
    Graph,
    dijkstra
) where

import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe)

-- Fibonacci Heap --------------------------------------------------------------

data Tree a = Tree {
    tKey      :: a,
    tRank     :: Int,
    tChildren :: [Tree a]
} deriving (Show)

data Heap a = Heap {
    minRoot :: Maybe (Tree a),
    roots   :: [Tree a]
} deriving (Show)

empty :: Heap a
empty = Heap Nothing []

singleton :: a -> Heap a
singleton x = insert x empty

insert :: Ord a => a -> Heap a -> Heap a
insert x (Heap Nothing rs) = Heap (Just t) (t:rs)
  where t = Tree x 0 []
insert x h@(Heap (Just m) rs)
    | x < tKey m = Heap (Just t) (t:rs)
    | otherwise  = Heap (Just m) (t:rs)
  where t = Tree x 0 []

findMin :: Heap a -> Maybe a
findMin (Heap Nothing _)   = Nothing
findMin (Heap (Just m) _) = Just (tKey m)

deleteMin :: Ord a => Heap a -> Maybe (a, Heap a)
deleteMin (Heap Nothing _) = Nothing
deleteMin (Heap (Just m) rs) = Just (tKey m, makeHeap (tChildren m ++ rs))

-- internal helpers -------------------------------------------------------------

makeHeap :: Ord a => [Tree a] -> Heap a
makeHeap ts = let rs = consolidate ts
                  (m, rest) = extractMin rs
               in Heap m rest

consolidate :: Ord a => [Tree a] -> [Tree a]
consolidate = IM.elems . foldl insertTree IM.empty
  where
    insertTree im t = case IM.lookup (tRank t) im of
        Nothing -> IM.insert (tRank t) t im
        Just t' -> insertTree (IM.delete (tRank t) im) (link t t')

link :: Ord a => Tree a -> Tree a -> Tree a
link t1 t2
    | tKey t1 <= tKey t2 = Tree (tKey t1) (tRank t1 + 1) (t2 : tChildren t1)
    | otherwise          = Tree (tKey t2) (tRank t2 + 1) (t1 : tChildren t2)

extractMin :: Ord a => [Tree a] -> (Maybe (Tree a), [Tree a])
extractMin [] = (Nothing, [])
extractMin (t:ts) = case extractMin ts of
    (Nothing, rest) -> (Just t, rest)
    (Just m, rest) ->
        if tKey t <= tKey m then (Just t, m:rest) else (Just m, t:rest)

-- Dijkstra ---------------------------------------------------------------------

type Graph v w = Map.Map v [(v, w)]

dijkstra :: (Ord v, Ord w, Num w) => Graph v w -> v -> Map.Map v w
dijkstra graph src = go (insert (0, src) empty) (Map.singleton src 0)
  where
    go heap distMap = case deleteMin heap of
        Nothing -> distMap
        Just ((d,u), heap')
            | Just best <- Map.lookup u distMap, d > best -> go heap' distMap
            | otherwise ->
                let neigh = Map.findWithDefault [] u graph
                    (heap'', distMap') = foldl (relax d) (heap', distMap) neigh
                 in go heap'' distMap'

    relax d (h, dm) (v,w) =
        let nd = d + w
        in case Map.lookup v dm of
            Nothing -> (insert (nd, v) h, Map.insert v nd dm)
            Just old -> if nd < old
                        then (insert (nd, v) h, Map.insert v nd dm)
                        else (h, dm)
