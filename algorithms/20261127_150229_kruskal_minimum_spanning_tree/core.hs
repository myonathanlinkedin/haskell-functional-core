module Core (
    Edge(..),
    Graph,
    DSU,
    emptyDSU,
    find,
    union,
    kruskalMST
) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (sortBy, foldl')
import Data.Function (on)

-- | Edge of an undirected weighted graph.
data Edge = Edge
    { weight :: Int   -- ^ Edge weight
    , from   :: Int   -- ^ Source vertex
    , to     :: Int   -- ^ Destination vertex
    } deriving (Eq, Show)

instance Ord Edge where
    compare = compare `on` weight

type Graph = [Edge]

-- | Pure Disjoint‑Set Union (Union‑Find) structure.
data DSU = DSU
    { parent :: IntMap Int   -- ^ Parent links
    , rank   :: IntMap Int   -- ^ Approximate tree depth
    } deriving (Show)

-- | Initialise a DSU where each element is its own set.
emptyDSU :: [Int] -> DSU
emptyDSU elems = DSU
    { parent = IntMap.fromList [(v, v) | v <- elems]
    , rank   = IntMap.fromList [(v, 0) | v <- elems]
    }

-- | Find the representative of an element with path compression.
find :: DSU -> Int -> (Int, DSU)
find dsu x = case IntMap.lookup x (parent dsu) of
    Nothing -> error ("find: unknown element " ++ show x)
    Just p  ->
        if p == x
            then (x, dsu)
            else
                let (root, dsu') = find dsu p
                    newParent = IntMap.insert x root (parent dsu')
                in (root, dsu' { parent = newParent })

-- | Union two sets by rank.
union :: DSU -> Int -> Int -> DSU
union dsu x y =
    let (rx, dsu1) = find dsu x
        (ry, dsu2) = find dsu1 y
    in if rx == ry
        then dsu2
        else
            let rankX = IntMap.findWithDefault 0 rx (rank dsu2)
                rankY = IntMap.findWithDefault 0 ry (rank dsu2)
            in if rankX < rankY
                then dsu2 { parent = IntMap.insert rx ry (parent dsu2) }
                else if rankX > rankY
                    then dsu2 { parent = IntMap.insert ry rx (parent dsu2) }
                    else dsu2
                        { parent = IntMap.insert ry rx (parent dsu2)
                        , rank   = IntMap.insert rx (rankX + 1) (rank dsu2)
                        }

-- | Kruskal's algorithm returning the MST edges and its total weight.
kruskalMST :: Int      -- ^ Number of vertices (assumed labelled 0..n-1)
           -> Graph    -- ^ Edge list
           -> ([Edge], Int)   -- ^ (MST edges, total weight)
kruskalMST n edges =
    let sorted = sortBy (compare `on` weight) edges
        dsu0   = emptyDSU [0 .. n-1]
        (mstRev, _, _) = foldl' step ([], dsu0, 0) sorted
    in (reverse mstRev, sum (map weight mstRev))
  where
    step :: ([Edge], DSU, Int) -> Edge -> ([Edge], DSU, Int)
    step (es, dsu, cnt) e
        | cnt == n - 1 = (es, dsu, cnt)   -- MST complete
        | otherwise =
            let (rx, dsu1) = find dsu (from e)
                (ry, dsu2) = find dsu1 (to e)
            in if rx == ry
                then (es, dsu2, cnt)      -- Edge would create a cycle
                else (e : es, union dsu2 rx ry, cnt + 1)
