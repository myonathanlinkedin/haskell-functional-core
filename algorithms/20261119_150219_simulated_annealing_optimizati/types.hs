module Types (
    Vertex,
    Weight,
    Edge,
    Graph(..),
    Solution(..)
) where

type Vertex = Int
type Weight = Double
type Edge = (Vertex, Vertex, Weight)

data Graph = Graph {
    numVertices :: Int,
    edges       :: [Edge]
} deriving (Show, Eq)

newtype Solution = Solution {
    assignment :: [Bool]   -- True/False indicates side of the cut
} deriving (Show, Eq)
