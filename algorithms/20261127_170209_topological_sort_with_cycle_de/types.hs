module Types where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- | Vertex identifier. Users may choose any orderable type.
type Vertex = Int

-- | Directed graph represented as an adjacency list.
--   For each vertex, the list contains its outgoing neighbours.
type Graph = Map Vertex [Vertex]
