module Core where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (foldl')
import Data.Maybe (fromMaybe)

type Node = Int
type Graph = Map.Map Node [Node]
type Value = Set.Set Int
type TransferFunction = Value -> Value
type Meet = Value -> Value -> Value

-- | Compute the fixed point of a data‑flow analysis using Kildall's algorithm.
-- The analysis is defined by:
--   * graph: control‑flow graph (node → successors)
--   * transfer: transfer function applied at each node
--   * meet: lattice meet operator
--   * initVal: initial value for nodes with no predecessors
kildall :: Graph -> TransferFunction -> Meet -> Value -> Map.Map Node Value
kildall graph transfer meet initVal = go initialMap (Map.keys graph)
  where
    preds :: Map.Map Node [Node]
    preds = buildPreds graph

    initialMap :: Map.Map Node Value
    initialMap = Map.fromList [(n, initVal) | n <- Map.keys graph]

    go :: Map.Map Node Value -> [Node] -> Map.Map Node Value
    go m [] = m
    go m (n:ws) =
      let predVals = [Map.findWithDefault initVal p m | p <- Map.findWithDefault [] n preds]
          meetVal  = foldl' meet initVal predVals
          newVal   = transfer meetVal
          oldVal   = Map.findWithDefault initVal n m
      in if newVal == oldVal
           then go m ws
           else let m'   = Map.insert n newVal m
                    succs = Map.findWithDefault [] n graph
                    ws'   = ws ++ succs
                in go m' ws'

    buildPreds :: Graph -> Map.Map Node [Node]
    buildPreds g = Map.foldlWithKey' addPreds Map.empty g
      where
        addPreds acc node succs = foldl' (\a s -> Map.insertWith (++) s [node] a) acc succs
