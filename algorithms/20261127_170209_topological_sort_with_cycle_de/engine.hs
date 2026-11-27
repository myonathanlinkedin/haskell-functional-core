module Engine (topologicalSort) where

import Types
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad.State.Strict (State, execState, get, modify)

-- | Result of topological sorting.
--   Left contains an error message (e.g., cycle detection),
--   Right contains a list of vertices in topological order.
type TopoResult = Either String [Vertex]

-- Internal visitation state for DFS.
data VisitState = Unvisited | Visiting | Visited
  deriving (Eq, Show)

type VisitMap = Map Vertex VisitState

-- | Perform a topological sort on a directed graph.
--   Returns either an error message (if a cycle exists) or a
--   list of vertices in topological order.
topologicalSort :: Graph -> TopoResult
topologicalSort graph = 
    case execState (mapM_ dfs (Map.keys graph)) (InitState Map.empty [] False) of
        InitState _ order True  -> Left "Graph contains a cycle; topological order not possible."
        InitState _ order False -> Right (reverse order)
  where
    -- Depth‑first search from a given vertex.
    dfs :: Vertex -> State InitState ()
    dfs v = do
        st <- get
        case Map.lookup v (visitMap st) of
            Just Visited  -> return ()
            Just Visiting -> modify (\s -> s { hasCycle = True })
            Nothing       -> do
                modify (\s -> s { visitMap = Map.insert v Visiting (visitMap s) })
                mapM_ dfs (Map.findWithDefault [] v graph)
                modify (\s -> s { visitMap = Map.insert v Visited (visitMap s)
                                , order    = v : order s })

-- | Internal state carried during DFS.
data InitState = InitState
    { visitMap :: VisitMap   -- ^ Tracking of each vertex's visitation status.
    , order    :: [Vertex]   -- ^ Accumulated topological order (built in reverse).
    , hasCycle :: Bool       -- ^ Flag set when a back‑edge (cycle) is discovered.
    }
