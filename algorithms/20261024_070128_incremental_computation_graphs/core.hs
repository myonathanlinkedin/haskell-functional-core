module Core (
    NodeId,
    Graph,
    emptyGraph,
    addNode,
    topologicalOrder,
    evaluate,
    incrementalUpdate
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import Data.List (foldl')
import Data.Maybe (fromMaybe)

type NodeId = Int

-- | A node contains its direct dependencies and a pure function that
--   computes its value from a map of already‑computed values.
data Node a = Node
    { nodeDeps :: [NodeId]
    , nodeFunc :: Map NodeId a -> a
    }

-- | The computation graph stores all nodes and a reverse‑dependency map
--   (for each node, the list of nodes that depend on it).
data Graph a = Graph
    { nodeMap :: Map NodeId (Node a)
    , revDeps :: Map NodeId [NodeId]   -- ^ dependents
    }

emptyGraph :: Graph a
emptyGraph = Graph Map.empty Map.empty

-- | Insert a node into the graph, updating reverse dependencies.
addNode :: NodeId -> [NodeId] -> (Map NodeId a -> a) -> Graph a -> Graph a
addNode nid deps f (Graph nmap rmap) =
    let nmap' = Map.insert nid (Node deps f) nmap
        rmap' = foldl' (\m d -> Map.insertWith (++) d [nid] m) rmap deps
    in Graph nmap' rmap'

-- | Compute a topological ordering of the graph using Kahn's algorithm.
--   Returns Left with an error message if a cycle is detected.
topologicalOrder :: Graph a -> Either String [NodeId]
topologicalOrder (Graph nmap _) = go initialZero Set.empty []
  where
    indegreeMap = Map.map (length . nodeDeps) nmap
    initialZero = Map.keysSet $ Map.filter (==0) indegreeMap

    go :: Set.Set NodeId -> Set.Set NodeId -> [NodeId] -> Either String [NodeId]
    go zero visited order
        | Set.null zero =
            if Map.size nmap == Set.size visited
                then Right (reverse order)
                else Left "Cycle detected in graph"
        | otherwise =
            let (n, restZero) = Set.deleteFindMin zero
                visited' = Set.insert n visited
                order' = n : order
                -- decrease indegree of dependents
                dependents = fromMaybe [] $ Map.lookup n (revDeps (Graph nmap Map.empty))
                (newZero, indegreeMap') = foldl' update (restZero, indegreeMap) dependents
            in go newZero visited' order'

    update (z, indeg) m =
        case Map.lookup m indeg of
            Just d | d > 1 -> (z, Map.insert m (d-1) indeg)
            Just _        -> (Set.insert m z, Map.delete m indeg)
            Nothing       -> (z, indeg)  -- should not happen

-- | Evaluate the whole graph given a map of initial values for source nodes.
--   Returns either an error (e.g., cycle) or the complete value map.
evaluate :: Graph a -> Map NodeId a -> Either String (Map NodeId a)
evaluate g@(Graph nmap _) initVals = do
    order <- topologicalOrder g
    let step vals nid =
            case Map.lookup nid nmap of
                Nothing -> vals  -- unknown node, ignore
                Just (Node deps f) ->
                    let depVals = Map.restrictKeys vals (Set.fromList deps)
                    in Map.insert nid (f depVals) vals
    return $ foldl' step initVals order

-- | Incrementally update the graph after a set of input changes.
--   Only nodes reachable from the changed inputs are recomputed.
incrementalUpdate :: Graph a -> Map NodeId a -> Map NodeId a -> Either String (Map NodeId a)
incrementalUpdate g@(Graph nmap rmap) oldVals changed = do
    order <- topologicalOrder g
    let baseVals = Map.union changed oldVals  -- changed overrides old
        -- collect all affected nodes via BFS on reverse dependencies
        bfs :: Set.Set NodeId -> Set.Set NodeId -> Set.Set NodeId
        bfs frontier visited
            | Set.null frontier = visited
            | otherwise =
                let n = Set.findMin frontier
                    frontier' = Set.delete n frontier
                    visited' = Set.insert n visited
                    next = Set.fromList $ fromMaybe [] (Map.lookup n rmap)
                    newFrontier = Set.union frontier' (Set.difference next visited')
                in bfs newFrontier visited'
        affected = bfs (Map.keysSet changed) Set.empty
        -- filter the global topological order to the affected subgraph
        affectedOrder = filter (`Set.member` affected) order
        recompute vals nid =
            case Map.lookup nid nmap of
                Nothing -> vals
                Just (Node deps f) ->
                    let depVals = Map.restrictKeys vals (Set.fromList deps)
                    in Map.insert nid (f depVals) vals
    return $ foldl' recompute baseVals affectedOrder
